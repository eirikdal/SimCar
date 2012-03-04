module PHEV

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
open MathNet.Numerics.Distributions
//open Node

//let syncContext = SynchronizationContext.CaptureCurrent()

let sum time profile = 
    match profile.dist_type with
    | Normal ->
        let n = new Normal(profile.mean, profile.sigma)
        n.CumulativeDistribution(time) - n.CumulativeDistribution((time-1.0))   
    | LogNormal ->
        let n = new Cauchy(profile.mean, profile.sigma)
        n.CumulativeDistribution(time) - n.CumulativeDistribution((time-1.0))

let sum_profile profiles time = 
    profiles
    |> Seq.fold (fun ac p -> ac + sum time p) 0.0 

let calc name profiles : Profile = 
    let profile = 
        (fun i -> sum_profile profiles (float i))
        |> Seq.initInfinite
        |> Seq.cache
        
    FloatProfile(name, profile)
(* 
 * PHEV: This is the PHEV agent
 *)
let phev_agent _p = Agent<'a Message>.Start(fun agent ->
    let rec loop (PHEV(phev_args) as phev) = async {
        let! msg = agent.Receive()

        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted<'a> (agent, sprintf "Agent %s says 'Hello, World!'" phev_args.name)

            return! loop phev
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(phev))

                return! loop phev
        | Model(phev) ->
            return! loop phev
        | Update(tick) ->
            match phev_args.profile with 
            | FloatProfile(name,dist_list) ->
                let r = new System.Random()

                let f = (Seq.nth tick dist_list)

                if phev_args.left < 0 then
                    if r.NextDouble() < f then
                        syncContext.RaiseEvent phevEvent <| sprintf "time %f, prob: %f" (float tick / 4.0) f
                    
                        return! loop <| PHEV({ phev_args with left=tick; duration=8 })
                    else if phev_args.battery < phev_args.capacity then
                        let phevArgs = { phev_args with current=phev_args.rate; battery=phev_args.battery+phev_args.current }

                        return! loop <| PHEV(phevArgs)
                    else if phev_args.battery >= phev_args.capacity then
                        let phevArgs = { phev_args with current=0.0<kWh> }
                    
                        return! loop <| PHEV(phevArgs)
                else
                    if tick = (phev_args.left + phev_args.duration) then
                        return! loop <| PHEV({ phev_args with battery=(phev_args.battery - phev_args.rate);left=(-1);duration=(-1) })

                    return! loop <| PHEV({ phev_args with battery=(phev_args.battery - phev_args.rate) })
            | DistProfile(name,dist_list) ->
                let p = calc name dist_list
                
                let phevArguments = { phev_args with profile=p }

                return! loop <| PHEV(phevArguments)
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")

            return! loop phev
        
        return! loop phev
    } 

    loop _p)
