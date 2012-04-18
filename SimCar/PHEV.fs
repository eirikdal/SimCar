module PHEV

#nowarn "25"

open System
open System.Collections
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
open PostalService
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
//open Node
let mutable rand = new System.Random()
//let syncContext = SynchronizationContext.CaptureCurrent()

module Action = 
    let send_intention (phev_args : PhevArguments) ttl =
        let intention, wait = 
            if phev_args.intentions.Length = 0 then
                Charge(phev_args.name, Energy.ofFloat (float (phev_args.capacity - phev_args.battery)), ttl, phev_args.rate), true
            else
                match phev_args.intentions with
                | h::t -> Charge_OK(phev_args.name, h, -1), false
                | [] -> Charge_OK(phev_args.name, 0.0<kWh>,-1), false

        postalService.send(phev_args.parent, intention)
        wait

    let charge (phev_args : PhevArguments) =
        // PHEV stayed home, charge battery if less than full
        PHEV(phev_args.charge())

    let leave name (phev_args : PhevArguments) tick =  
        // If first time running, calculate and cache the distributions
        let ({ profile=(FloatProfile(_,dist_list)) } as phevArgs) = 
            { phev_args with profile=phev_args.profile.float_profile() }

        let r = rand.NextDouble()
        // try to find a distribution that matches the random number r
        let dist = dist_list |> Seq.tryFind (fun ({dist=dist}) -> r < (Seq.nth (tick%96) dist))
                       
        // if a distribution was found, let the PHEV leave with the corresponding duration of the distribution
        match dist with
        | Some d ->   
            //syncContext.RaiseEvent jobDebug <| sprintf "PHEV %s left with chance %f and probability %f" name r (Seq.nth (tick%96) d.dist) 
            PHEV(phevArgs.leave(tick, d.duration))
        | None ->
            PHEV({ phevArgs with left=(-1);})

    let find_ttl (histogram : int array) tick =
        let tick' = (tick%96)
        let nbhood = (tick'+ 10)

        // finding the mode
        [for i in tick' .. nbhood do yield histogram.[(i%96)]] 
        |> List.fold (fun ((i, cur), ac) x -> if x > ac then ((i+1,i),x) else ((i+1,cur),ac)) ((0,0),0) 
        |> fst 
        |> snd
        |> (+) tick


(* 
 * PHEV: This is the PHEV agent
 *)
let phev_agent _p name = Agent<Message>.Start(fun agent ->
    let queue = new Queue() 
    let test = new MathNet.Numerics.Statistics.Histogram()
    
    let rec loop (PHEV({ name=name; parent=parent; histogram=histogram } as phev_args) as phev) waiting = async {
        let! (msg : Message) = 
            if not waiting && queue.Count > 0 then
                async { return queue.Dequeue() :?> Message }
            else
                agent.Receive()
        match msg with
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                if not waiting then
                    reply.Reply(Model(phev))
                    return! loop phev false
                else
                    queue.Enqueue(msg)
                    return! loop phev waiting
        | Model(phev) ->
            return! loop phev waiting
        | Charge_Accepted(accepted) ->
            let phevArgs = { phev_args with intentions=accepted }
            
            let wait_for_reply = Action.send_intention phevArgs (-1)
            
            return! loop (PHEV(phevArgs)) wait_for_reply
        | Charge_OK(_,_,_) ->
            return! loop (Action.charge phev_args) false
        | Update(tick) ->
            let ttl = Action.find_ttl histogram tick 
            let wait_for_reply = Action.send_intention phev_args ttl
            
            if phev_args.name = "phev_192" then
                syncContext.RaiseDelegateEvent phevBattery phev_args.battery
                if phev_args.duration > 0 then
                    syncContext.RaiseDelegateEvent phevStatus 1.0
                else 
                    syncContext.RaiseDelegateEvent phevStatus 0.0

            if phev_args.duration <= 0 then
                return! loop <| Action.leave name phev_args tick <| true
            else
                return! loop <| PHEV(phev_args.drive()) <| true
        | Reset -> 
            return! loop <| PHEV({ phev_args with battery=phev_args.capacity; duration=(-1) }) <| false
        | Kill ->
            printfn "Agent %s: Exiting.." name
        | _ -> 
            syncContext.RaiseEvent error <| Exception("PHEV: Not implemented yet")

            return! loop phev waiting }
    loop _p false)
