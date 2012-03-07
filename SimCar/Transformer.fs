module Transformer

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Message
open Agent
open Models
open PostalService
(* 
    Transformer: This is the transformer agent
*)

let trf_agent trf = Agent.Start(fun agent ->
    let rec loop (Transformer({ parent=parent } as trf_args)) = async {
        let! msg = agent.Receive()
        
        match msg with
        | Hello ->
            syncContext.RaiseEvent jobCompleted<_> (agent, sprintf "Agent %s says 'Hello, World!'" trf_args.name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(trf))
        | Model(trf) -> 
            return! loop trf
        | Update(tick) ->
            ()
        | Charge(name, energy) as msg ->
            postalService.send(parent, msg)
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Trf: Not yet implemented")

        jobCompleted.Publish |> ignore

        return! loop trf
    }
    
    loop trf)
