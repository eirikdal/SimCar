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

// pre-calculate distributions
let calc name (profiles : Distribution list) : Profile =         
    let dist_list = 
        profiles
        |> List.map (fun p -> { p with dist=(fun i -> sum (float i) p) |> Seq.initInfinite |> Seq.cache })
        
    FloatProfile(name, dist_list)
(* 
 * PHEV: This is the PHEV agent
 *)
let phev_agent _p name = Agent<'a Message>.Start(fun agent ->
    let rec loop (PHEV({parent=parent} as phev_args) as phev) = async {
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
            | FloatProfile(dist_name,dist_list) ->
                // if PHEV is at home, see if it is time to leave, reduce duration and state of battery to account for travel time
                if phev_args.left < 0 then
                    // testing broadcast intention
                    if phev_args.battery < phev_args.capacity then
                        postalService.send(parent, Charge(name, Energy.ofFloat (float (phev_args.capacity - phev_args.battery))))
//                    let test = Charge(name, Energy.ofFloat (float phev_args.left))
                    let r = (new System.Random()).NextDouble()
//                    printfn "%s" parent
                    // try to find a distribution that matches the random number r
                    let dist = dist_list |> Seq.tryFind (fun ({dist=dist}) -> r < (Seq.nth tick dist))
                    
                    // if a distribution was found, let the PHEV leave with the corresponding duration of the distribution
                    match dist with
                    | Some d ->
                        syncContext.RaiseEvent phevEvent <| sprintf "time %f, duration %d" (float tick / 4.0) d.duration
                        
                        return! loop <| PHEV({ phev_args with left=tick; duration=d.duration })
                    | None ->
                        // PHEV stayed home, charge battery if less than full
                        if phev_args.battery < phev_args.capacity then
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
                // First time running the distribution profile, calculate and cache the distributions
                let p = calc name dist_list
                
                let phevArguments = { phev_args with profile=p }

                return! loop <| PHEV(phevArguments)
        | _ -> 
            syncContext.RaiseEvent error <| Exception("PHEV: Not implemented yet")

            return! loop phev
        
        return! loop phev
    } 

    loop _p)
