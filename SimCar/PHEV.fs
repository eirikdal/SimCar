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
        
        let test = n.CumulativeDistribution(time*15.0) - n.CumulativeDistribution((time-1.0)*15.0)
        if test > 0.0 then
            printfn "%f: %f" time test
        test
    | LogNormal ->
        let n = new Cauchy(profile.mean, profile.sigma)
//        MathNet.Numerics.Distributions.log
        let test = n.CumulativeDistribution(time*15.0) - n.CumulativeDistribution((time-1.0)*15.0)
        if test > 0.0 then
            printfn "%f: %f" time test
        test

let sum_profile profiles time = 
    profiles
    |> Seq.fold (fun ac p -> ac + sum time p) 0.0 

let calc name profiles : Profile = 
    let profile = 
        Seq.init (96) (fun i -> float i)
        |> Seq.map (fun x -> sum_profile profiles x)
        |> Seq.cache
        
    FloatProfile(name, profile)
(* 
    PHEV: This is the PHEV agent
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
        | Update(tick) ->
            match phev_args.profile with 
            | FloatProfile(name,dist_list) ->
                let r = new System.Random()
                
                let f = (Seq.nth (tick%96) dist_list)
                if r.NextDouble() < f then
                    printfn "time %f, prob: %f" (float tick%96.0 / 4.0) f

                return! loop phev
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
