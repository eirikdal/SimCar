module PHEV

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
open PostalService
open MathNet.Numerics.Distributions
//open Node

//let syncContext = SynchronizationContext.CaptureCurrent()

module Action = 
    let sum time profile = 
        match profile.dist_type with
        | Normal ->
            let n = new Normal(profile.mean, profile.sigma)
            n.CumulativeDistribution(time) - n.CumulativeDistribution((time-1.0))   
        | LogNormal ->
            let n = new MathNet.Numerics.Distributions.Gamma(profile.mean, 7.0)
            n.CumulativeDistribution(time+1.0) - n.CumulativeDistribution((time))

    // cache the distributions
    let calc name (profiles : Distribution list) : Profile =         
        let dist_list = 
            profiles
            |> List.map (fun p -> { p with dist=(fun i -> sum (float i) p) |> Seq.initInfinite |> Seq.take 96 |> Seq.cache })
    
        // calculate the accumulated probability density function of all distributions
        let prob ({dist=dist}) i = dist |> Seq.nth (i%96)
        let temp = Seq.initInfinite (fun i -> dist_list |> Seq.fold (fun ac (d : Distribution) -> ac + (prob d i)) 0.0) |> Seq.take 96 |> Array.ofSeq

        probEvent.Trigger [|box temp; box System.EventArgs.Empty|]

        FloatProfile(name, dist_list)

    let charge phev_args accepted =
        // PHEV stayed home, charge battery if less than full
        if phev_args.battery < phev_args.capacity then
            let phevArgs = { phev_args with current=accepted; battery=phev_args.battery+accepted }
            
            PHEV(phevArgs)
        else
            let phevArgs = { phev_args with current=0.0<kWh> }
                    
            PHEV(phevArgs)

    let leave dist_list phev_args tick accepted =  
        let r = (new System.Random()).NextDouble()
        // try to find a distribution that matches the random number r
        let dist = dist_list |> Seq.tryFind (fun ({dist=dist}) -> r < (Seq.nth (tick%96) dist))
                       
        // if a distribution was found, let the PHEV leave with the corresponding duration of the distribution
        match dist with
        | Some d ->   
            syncContext.RaiseEvent jobDebug <| "PHEV left"
            PHEV({ phev_args with left=(tick%96); duration=d.duration;current=Energy.ofFloat 0.0; })
        | None ->
            charge phev_args accepted
    

(* 
 * PHEV: This is the PHEV agent
 *)
let phev_agent _p name = Agent<Message>.Start(fun agent ->
    let rec loop (PHEV({name=name; parent=parent} as phev_args) as phev) = async {
        let! msg = agent.Receive()

        match msg with
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(phev))
                
                return! loop phev
        | Model(phev) ->
            return! loop phev
        | Update(tick) ->
            if name = "Godel" then
                syncContext.RaiseDelegateEvent phevBattery phev_args.battery
            match phev_args.profile with 
            | FloatProfile(dist_name,dist_list) ->
                if phev_args.duration <= 0 then
                    if name = "Godel" then
                        syncContext.RaiseDelegateEvent phevStatus 0.0
                    // if PHEV is at home, see if it is time to leave
                    let intention = Charge(name, Energy.ofFloat (float (phev_args.capacity - phev_args.battery)), 30, phev_args.rate)
//                    printfn "%s sending charge to %s" name parent
                    let charge = postalService.send_reply(parent, intention)
                    let accepted = 
                        match charge with
                        | Charge_Accepted(accepted) -> accepted

                    return! loop <| Action.leave dist_list phev_args tick accepted
                else
                    if name = "Godel" then
                        syncContext.RaiseDelegateEvent phevStatus 1.0
                    if phev_args.duration = 1 then
                        let intention = Charge(name, Energy.ofFloat (float (phev_args.capacity - phev_args.battery)), 30, phev_args.rate)
//                        printfn "%s sending charge to %s" name parent

                        let reply = postalService.send_reply(parent, intention)
                        let accepted = 
                            match reply with
                            | Charge_Accepted(accepted) -> accepted

                        let phevArgs = { phev_args with battery=(phev_args.battery + accepted);duration=phev_args.duration-1 }
                    
                        return! loop <| Action.leave dist_list phevArgs tick accepted
                    else
                        let intention = Charge_OK(name)
                        postalService.send(parent, intention)

                        return! loop <| PHEV({ phev_args with battery=(phev_args.battery - phev_args.rate); duration=phev_args.duration-1 })

            | DistProfile(_,dist_list) ->
                // First time running the distribution profile, calculate and cache the distributions
                let p = Action.calc name dist_list
                let intention = Charge_OK(name)
                        
                postalService.send(parent, intention)
                let phevArguments = { phev_args with profile=p }

                return! loop <| PHEV(phevArguments)
        | _ -> 
            syncContext.RaiseEvent error <| Exception("PHEV: Not implemented yet")

            return! loop phev
        
        return! loop phev
    } 

    loop _p)
