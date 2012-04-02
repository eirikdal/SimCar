#r "bin/Debug/SimCar.dll"
#load "FSharpChart.fsx"

#nowarn "25"

open MSDN.FSharp.Charting
open DayAhead
open PostalService
open System.Collections.Generic
open Models
open System
open FileManager
open Agent

let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"
let interpol_folder = "C:\\SimCar\\SimCar\\data\\interpol\\"

let profiles_interpol = Parsing.parse_powerprofiles(interpol_folder)

let map2 op list1 list2 = list1 |> List.map2 (fun sum t -> if (op sum t) < 1.0 then sum+t else 1.0) list2
let sum2 (list1 : float list) (list2 : float list) = list1 |> List.map2 (fun sum t -> sum+t) list2
let sumn list = list |> List.fold (fun ac dist -> sum2 ac dist) (List.init (96) (fun _ -> 0.0))

let realtime' = Array.sub (snd profiles_interpol.Head) 0 96
let mutable realtime = (realtime'.Clone() :?> float array)

let powergrid = 
    FileManager.powergrid()

let collect_exp node = 
    match node with
    | Transformer(_) -> []
    | PHEV(phev_args) as node -> phev_args.profile.to_exp_float(2.5)
    | PowerNode(_) -> []
    | BRP(_) -> []

let mutable agg_dist' = 
    powergrid
    |> Tree.map collect_exp 
    |> Tree.collect
    |> List.ofSeq
    |> List.filter (fun x -> if x.Length = 0 then false else true)
    |> List.map (fun dist -> sumn dist)
    |> sumn
    |> Array.ofList

let mutable agg_dist : float array = (agg_dist'.Clone() :?> float array)

//let exp_load = 
//    map2 (*) realtime agg_dist

// 1. find phev profiles
// 2. add expected load to realtime
// 3. initialize swarm to find peaks and sinks
// 4. distribute load to sinks

//let test = FSharpChart.StepLine(agg_dist)

type Message = 
    | Init of int * int
    | Moved of int * int
    | Fill of float * AsyncReplyChannel<Message>
    | FillQuery of float * int
    | Utility of int * float
    | RequestChart of AsyncReplyChannel<Message>
    | Chart of float array
    | Occupied
    | Accepted
    | Exit

let n = 9

type Agent<'T> = MailboxProcessor<'T>

let peak = realtime |> Array.max
let init_pos = realtime |> Array.findIndex (fun x -> x = peak)

let sum_agg_dist = agg_dist |> Array.sum

let rate = 1.25

// remember, remember, the 5th of november


// let supervisor agent iterate over expected load:
// for each point, distribute expected load for that point,
// letting the utility of an agent depend on the
// distance of its position from the point to distribute

let rec ants = 
    [for i in 0 .. n do 
        yield Agent<Message>.Start(fun agent ->
           
        let rec loop n pos = async {
            let! msg = agent.Receive()

            match msg with
            | Exit -> printfn "Agent %d signing off!" n; 
            | Init(pos, inertia) -> 
                printfn "Agent %d initialized with position %d" n pos
                
                return! moving n pos inertia
            | _ -> return! loop n pos }
        and moving n pos inertia = async {
            if realtime.[pos+inertia] < realtime.[pos] then 
                return! moving n (pos+inertia) inertia
            else
                supervisor.Post(Moved(n, pos))
                return! waiting n pos inertia }
        and waiting n pos inertia = 
            agent.Scan(function 
                | Occupied -> 
                    Some(async { return! moving n (pos+inertia) inertia })
                | Accepted -> 
                    Some(async { return! filling n pos inertia })
                | _ -> None)
        and filling id pos inertia = 
            agent.Scan(function
                | FillQuery(rate, pos') -> 
                    let distance = 0.5 ** (float <| abs(pos-pos'))
                    supervisor.Post(Utility(id, distance*(1.0 / realtime.[pos])))
                    Some(async { return! filling id pos inertia })
                | Fill(rate, reply) ->
                    printfn "Updating realtime with %f at %d" rate pos
                    realtime.[pos] <- realtime.[pos] + rate
                    let pos', inertia' = 
                        if (pos+inertia) < realtime.Length && (pos+inertia > 0) && realtime.[pos+inertia] < realtime.[pos] then
                            pos+inertia, inertia
                        else
                            pos, (inertia*(-1))
                    reply.Reply(Moved(id, pos'))
                    Some(async { return! filling id pos' inertia' })
                | Exit -> Some( async { () } ))
        loop i 0)]
and supervisor : Agent<Message> = 
    let agent_pos = Array.init (ants.Length) (fun _ -> -1)

    Agent<Message>.Start(fun agent ->
        ants |> List.iteri (fun i ant -> 
            let inertia = if i % 2 = 0 then 1 else -1
            ant.Post(Init(init_pos, inertia)))

        let rec loop() = async {
            let! msg = agent.Receive()

            match msg with
            | Moved(ag, pos) ->
                let d = agent_pos |> Array.tryFindIndex (fun p -> abs(pos-p) < 5)

                match d with
                | Some _ -> 
                    ants.[ag].Post(Occupied)
                | None -> 
                    printfn "Agent %d in position %d" ag pos
                    agent_pos.[ag] <- pos
                    ants.[ag].Post(Accepted)

                let all_in_pos = agent_pos |> Array.forall (fun p -> p >= 0)

                if all_in_pos then 
                    printfn "All agents in position" 
                    return! filling agg_dist.[0] 0
                else
                    return! loop()
            | RequestChart(reply) -> reply.Reply(Chart(realtime))
            | _ ->
                raise <| Exception("ffs")
            
            return! loop() }
        and filling remaining pos = async { 
            if remaining > 0.0 then
//                let rate' = if remaining > rate then rate else rate-remaining 
                ants |> List.iter (fun ant -> ant.Post(FillQuery(rate, pos)))
                printfn "Sending fillqueries for %d" pos
                return! waiting remaining [] pos
            else if (pos+1) < agg_dist.Length then
                printfn "Advancing agg_dist to %d" (pos+1)
                return! filling (agg_dist.[pos+1]) (pos+1)
            else 
                printfn "Filling complete"
                ants |> List.iter (fun ant -> ant.Post(Exit))
                
                return! idle() }
        and waiting remaining (agent_responses : 'a list) pos = 
            if agent_responses.Length = ants.Length then 
                let (Utility(id,_)) = agent_responses |> List.maxBy(function | Utility(_,v) -> v)
                let (Moved(n,pos')) = ants.[id].PostAndReply(fun reply -> (Fill(rate, reply)))
                agent_pos.[n] <- pos'
                agg_dist.[pos] <- (remaining-rate)
                async { return! filling (remaining-rate) pos }
            else
                agent.Scan(function 
                | Utility(_,_) as msg -> Some( async { return! waiting remaining (msg :: agent_responses) pos } )
                | _ -> None)
        and idle() = 
            agent.Scan(function 
                | RequestChart(reply) -> reply.Reply(Chart(realtime)); Some(async {()})
                | _ -> None)
        loop())

let (Chart(chart : float array)) = supervisor.PostAndReply(fun reply -> RequestChart(reply))

FSharpChart.Combine(
    [FSharpChart.StepLine(chart);
        FSharpChart.StepLine(agg_dist');
        FSharpChart.StepLine(realtime')])
