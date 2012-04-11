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
    | PHEV(phev_args) as node -> phev_args.profile.to_exp_float(Energy.toFloat <| phev_args.rate)
    | PowerNode(_) -> []
    | BRP(_) -> []

let mutable agg_dist' = 
    powergrid
    |> Tree.map collect_exp 
    |> Tree.collect
    |> List.ofSeq
    |> List.filter (fun x -> if x.Length = 0 then false else true)
    |> List.sumn
    |> Array.ofList

let mutable agg_dist : float array = (agg_dist'.Clone() :?> float array)

type Message = 
    | Init of int 
    | Moved of int * int
    | Fill of float
    | FillQuery of float * int
    | Utility of int * float
    | RequestChart of AsyncReplyChannel<Message>
    | Chart of float array
    | Occupied
    | Accepted
    | Failed of int
    | Exit

let num_agents = 95
let theta = 0.98
let sum_agg_dist = agg_dist |> Array.sum

let rate = 1.25

module Swarm = 
    let init (realtime : float array) = 
        let rand = new System.Random()
        let rec ants = 
            [for i in 0 .. num_agents do 
                yield Agent<Message>.Start(fun agent ->
                let rec loop n pos = agent.Scan((function
                    | Init(pos) -> 
                        Some( async { return! filling n pos } )
                    | _ -> None), 10000)
                and filling id pos = 
                    agent.Scan((function
                        | FillQuery(rate, pos') -> 
                            let distance = theta ** (float <| abs(pos-pos'))
                            supervisor.Post(Utility(id, distance*(1.0 / realtime.[pos])))
                            Some(async { return! filling id pos })
                        | Fill(rate) ->
                            realtime.[pos] <- realtime.[pos] + rate
                            syncContext.RaiseDelegateEvent dayaheadStep (realtime.Clone())

                            Some(async { return! filling id pos })
                        | Exit -> Some( async { () } )),10000)
                loop i 0)]
        and supervisor : Agent<Message> = 
            let agent_pos = Array.init (ants.Length) (fun _ -> -1)
    
            let peak = realtime |> Array.max
            let init_pos = realtime |> Array.findIndex (fun x -> x = peak)
//            let rand = new System.Random()
            Agent<Message>.Start(fun agent ->
                ants |> List.iteri (fun i ant -> 
                    let inertia = if i % 2 = 0 then 1 else -1
                    ant.Post(Init(i)))
                
                let rec loop() = async {
                    return! filling agg_dist'.[0] 0 }
                and filling remaining pos = async { 
                    if remaining > rate then
                        let rate' = if remaining > rate then rate else rate-remaining 
                        ants |> List.iter (fun ant -> ant.Post(FillQuery(rate', pos)))
                        return! waiting remaining [] pos
                    else if (pos+1) < agg_dist'.Length then
                        return! filling (agg_dist'.[pos+1]) (pos+1)
                    else 
                        ants |> List.iter (fun ant -> ant.Post(Exit))
                
                        return! idle() }
                and waiting remaining (agent_responses : 'a list) pos = 
                    if agent_responses.Length = ants.Length then 
                        let (Utility(id,_)) = agent_responses |> List.maxBy(function | Utility(_,v) -> v)
                        let rate' = if remaining > rate then rate else rate-remaining 
                        agg_dist'.[pos] <- remaining - rate'
                        syncContext.RaiseDelegateEvent dayaheadExpected (agg_dist'.Clone() :?> float array)
                        ants.[id].Post(Fill(rate'))

                        async { return! filling (remaining-rate') pos }
                    else
                        agent.Scan((function 
                            | Utility(_,_) as msg -> Some( async { return! waiting remaining (msg :: agent_responses) pos } )
                            | _ -> None), 10000)
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
            agg_dist' <- (agg_dist.Clone() :?> float array)
            let _from,_to = (i*96),(i*96)+96
            let day = Array.sub realtime _from 96
            let (supervisor, ants) = Swarm.init(day)
            let (Chart(chart : float array)) = supervisor.PostAndReply(fun reply -> RequestChart(reply))
            supervisor.Post(Exit)
            yield! (List.ofArray chart)]

    days |> Array.ofList