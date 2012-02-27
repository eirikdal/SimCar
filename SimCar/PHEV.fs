module PHEV

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
//open Node

//let syncContext = SynchronizationContext.CaptureCurrent()

let calc name profile : Profile = 
    
    FloatProfile(name, [])
(* 
    PHEV: This is the PHEV agent
*)
let phev_agent phev = Agent.Start(fun agent ->
    let rec loop (PHEV(phev_args)) = async {
        let! msg = agent.Receive()
        
        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted<_> (agent, sprintf "Agent %s says 'Hello, World!'" phev_args.name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(phev))
        | Update(tick) ->
            match phev_args.profile with 
            | FloatProfile(name,dist_list) ->
                let p = calc name dist_list
                
                let phevArguments = { phev_args with profile=p}

                return! loop <| PHEV(phevArguments)
            | DistProfile(_,dist_list) ->
                ()
            ()
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Not implemented yet")
        
        return! loop phev
    }
    
    

    loop phev)
