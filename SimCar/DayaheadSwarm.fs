module DayaheadSwarm

open System.Collections.Generic
open Models
open SynchronizationContext
open System
open FileManager
open Agent

#nowarn "25"
#nowarn "40"

let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"
let interpol_folder = "C:\\SimCar\\SimCar\\data\\interpol\\"

let profiles_interpol = Parsing.parse_powerprofiles(interpol_folder)

let powergrid = 
    FileManager.powergrid()

let collect_exp node = 
    match node with
    | Transformer(_) -> []
    | PHEV(phev_args) as node -> phev_args.profile.to_exp_float(1.25<kWh>, 32.0<kWh>)
    | PowerNode(_) -> []
    | BRP(_) -> []

let mutable agg_dist' = 
    powergrid
    |> Tree.map collect_exp 
    |> Tree.collect
    |> List.ofSeq
    |> List.filter (fun x -> if x.Length = 0 then false else true)
//    |> List.map (fun dist -> sumn dist)
    |> List.sumn
    |> Array.ofList

let mutable agg_dist : float array = (agg_dist'.Clone() :?> float array)

type Message = 
    | Init of int * int
    | Moved of int * int
    | Fill of float<kWh>
    | FillQuery of float<kWh> * int
    | Utility of int * float
    | RequestChart of AsyncReplyChannel<Message>
    | Chart of float<kWh> array
    | Occupied
    | Accepted
    | Failed of int
    | Exit

let num_agents = 20
let theta = 0.99
let sum_agg_dist = agg_dist |> Array.sum

let rate = 1.25<kWh>

module Swarm = 
    let init (realtime : float<kWh> array) = 
        let rand = new System.Random()
        let rec ants = 
            [for i in 0 .. num_agents do 
                yield Agent<Message>.Start(fun agent ->
                let rec loop n pos = agent.Scan((function
                    | Init(pos, inertia) -> 
//                        printfn "Agent %d initialized with position %d" n pos
                
                        Some( async { return! moving n pos inertia } )
                    | _ -> None), 10000)
                and moving n pos inertia = async {
                    if (pos+inertia) < realtime.Length && (pos+inertia) > 0 && realtime.[pos+inertia] < realtime.[pos] then 
                        return! moving n (pos+inertia) inertia
                    else
                        supervisor.Post(Moved(n, pos))
                        return! waiting n pos inertia }
                and waiting n pos inertia = 
                    agent.Scan((function 
                        | Occupied -> 
                            if (pos+inertia) >= realtime.Length || (pos+inertia) < 0 then
//                                supervisor.Post(Failed(n))
                                Some(async { return! moving n (rand.Next(0,95)) (rand.Next(-1,1)) })
                            else
                                Some(async { return! moving n (pos+inertia) inertia })
                        | Accepted -> 
                            Some(async { return! filling n pos inertia })
                        | _ -> None),10000)
                and filling id pos inertia = 
                    agent.Scan((function
                        | FillQuery(rate, pos') -> 
                            let distance = theta ** (float <| abs(pos-pos'))
                            supervisor.Post(Utility(id, distance*(1.0<kWh> / realtime.[pos])))
                            Some(async { return! filling id pos inertia })
                        | Fill(rate) ->
//                            printfn "Updating realtime with %f at %d" rate pos
                            realtime.[pos] <- realtime.[pos] + rate
                            syncContext.RaiseDelegateEvent dayaheadStep (realtime.Clone())

//                            let pos', inertia' = 
//                                if (pos+inertia) < realtime.Length && (pos+inertia > 0) && realtime.[pos+inertia] < realtime.[pos] then
//                                    pos+inertia, inertia
//                                else
//                                    pos, (inertia*(-1))
                            
//                            reply.Reply(Moved(id, pos'))
                            Some(async { return! moving id pos inertia })
                        | Exit -> Some( async { () } )),10000)
                and failed id = 
                    agent.Scan((function 
                        | Exit -> Some(async { () })
                        | FillQuery(_,_) -> 
                            supervisor.Post(Utility(id, -1.0))
                            Some(async { return! failed id })
                        | Init(_,_) ->
                            Some(async { return! failed id })
                        | _ -> None), 10000)
                loop i 0)]
        and supervisor : Agent<Message> = 
            let agent_pos = Array.init (ants.Length) (fun _ -> -1)
    
            let peak = realtime |> Array.max
            let init_pos = realtime |> Array.findIndex (fun x -> x = peak)
    
            Agent<Message>.Start(fun agent ->
                ants |> List.iteri (fun i ant -> 
                    let inertia = if i % 2 = 0 then 1 else -1
                    ant.Post(Init(init_pos, inertia)))
                
                let rec loop() = 
                    agent.Scan(fun x ->
                    let all_in_pos agent_pos = 
                        let all_in_pos = agent_pos |> Array.forall (fun p -> p = (int infinity) || p >= 0)

                        if all_in_pos then 
//                            printfn "All agents in position" 
                            Some(async { return! filling agg_dist'.[0] 0 })
                        else
                            Some(async { return! loop() }) 
                    match x with
                    | Moved(ag, pos) ->
                        let d = agent_pos |> Array.tryFindIndex (fun p -> p <> (int infinity) && abs(pos-p) < 5)

                        match d with
                        | Some _ -> 
                            ants.[ag].Post(Occupied)
                        | None -> 
                            agent_pos.[ag] <- pos
                            ants.[ag].Post(Accepted)

                        syncContext.RaiseDelegateEvent dayaheadAnt (agent_pos.Clone() :?> int array, realtime.Clone() :?> float array)

                        all_in_pos agent_pos
                    | Failed(ag) -> 
                        agent_pos.[ag] <- int infinity

                        all_in_pos agent_pos
                    | _ -> None)
                and filling remaining pos = async { 
                    if remaining > rate then
                        let rate' = if remaining > rate then rate else rate-remaining 
                        ants |> List.iter (fun ant -> ant.Post(FillQuery(rate', pos)))
//                        printfn "Sending fillqueries for %d" pos
                        return! waiting remaining [] pos
                    else if (pos+1) < agg_dist'.Length then
                        return! filling (agg_dist'.[pos+1]) (pos+1)
                    else 
//                        printfn "Dayahead complete"
                        ants |> List.iter (fun ant -> ant.Post(Exit))
                
                        return! idle() }
                and waiting remaining (agent_responses : 'a list) pos = 
                    if agent_responses.Length = ants.Length then 
                        let (Utility(id,_)) = agent_responses |> List.maxBy(function | Utility(_,v) -> v)
                        let rate' = if remaining > rate then rate else rate-remaining 
                        agg_dist'.[pos] <- remaining - rate'
                        syncContext.RaiseDelegateEvent dayaheadExpected (agg_dist'.Clone() :?> float array)
                        ants.[id].Post(Fill(rate'))

                        async { return! prep (remaining-rate') pos }
                    else
                        agent.Scan((function 
                            | Utility(_,_) as msg -> Some( async { return! waiting remaining (msg :: agent_responses) pos } )
                            | _ -> None), 10000)
                and prep remaining pos = agent.Scan((function 
                    | Moved(ag, pos') ->
                        let d = agent_pos |> Array.tryFindIndex (fun p -> p <> (int infinity) && p <> agent_pos.[ag] && abs(pos'-p) < 2)

                        match d with
                        | Some _ -> 
                            ants.[ag].Post(Occupied)
                            Some(async { return! prep remaining pos })
                        | None -> 
                            agent_pos.[ag] <- pos'
                            ants.[ag].Post(Accepted)

                            syncContext.RaiseDelegateEvent dayaheadAnt (agent_pos.Clone() :?> int array, realtime.Clone() :?> float array)

                            Some(async { return! filling remaining pos })
                    | Failed(ag) -> agent_pos.[ag] <- int infinity; Some(async { return! filling remaining pos })
                    | _ -> None),10000)
                and idle() = agent.Scan((function 
                    | RequestChart(reply) -> reply.Reply(Chart(realtime)); Some(async {()})
                    | Exit -> Some(async { () })
                    | _ -> None),10000)
                loop())
        (supervisor, ants)

let dayahead(realtime, n) =     
    let peak = realtime |> Array.max
    let init_pos = realtime |> Array.findIndex (fun x -> x = peak)

//    syncContext.RaiseDelegateEvent dayaheadExpected agg_dist'

    let days = 
        [for i in 0 .. (n-1) do
            agg_dist' <- (agg_dist.Clone() :?> float<kWh> array)
            let _from,_to = (i*96),(i*96)+96
            let day = Array.sub realtime _from 96
            let (supervisor, ants) = Swarm.init(day)
            let (Chart(chart : float<kWh> array)) = supervisor.PostAndReply(fun reply -> RequestChart(reply))
            supervisor.Post(Exit)
            yield! (List.ofArray chart)]

    days |> Array.ofList