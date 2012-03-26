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
    let send_intention name parent (phev_args : PhevArguments) ttl =
        let intention, wait = 
            if phev_args.duration <= 1 && phev_args.intentions.Length = 0 then
                Charge(name, Energy.ofFloat (float (phev_args.capacity - phev_args.battery)), ttl, phev_args.rate), true
            else
                Charge_OK(name), false

        postalService.send(parent, intention)
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
            
            if phevArgs.intentions.Length > 0 then
                return! loop (Action.charge phevArgs) false
            else
                return! loop (PHEV(phevArgs)) false
        | Update(tick) ->
            let ttl = Action.find_ttl histogram tick 
            let wait_for_reply = Action.send_intention name parent phev_args ttl
            let phevArgs = 
                if phev_args.intentions.Length = 0 then
                    phev_args 
                else
                    phev_args.charge()

            if phevArgs.duration <= 1 then
                return! loop <| Action.leave name phevArgs tick <| wait_for_reply
            else
                return! loop <| PHEV(phevArgs.drive()) <| wait_for_reply
        | Reset -> 
            return! loop <| PHEV({ phev_args with battery=phev_args.capacity; duration=(-1) }) <| false
        | _ -> 
            syncContext.RaiseEvent error <| Exception("PHEV: Not implemented yet")

            return! loop phev waiting
        
        return! loop phev waiting } 
    and waiting() = async {
        ()
    }

    loop _p false)
