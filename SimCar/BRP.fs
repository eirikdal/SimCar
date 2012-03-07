module BRP

#nowarn "25"

open Agent
open Message
open Models
open System
open System.Threading
open SynchronizationContext

let brp_agent brp = Agent.Start(fun agent ->
    let rec loop (BRP(brp_args)) = async {
        let! msg = agent.Receive()

        match msg with
        | Hello -> 
            syncContext.RaiseEvent jobCompleted<_> (agent, sprintf "Agent %s says 'Hello, World!'" brp_args.name)
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(brp))
        | Update(tick) -> 
            ()
        | Charge(name, energy) -> 
            ()
//            syncContext.RaiseEvent progress <| sprintf "Charge intention from agent %s: %f kWh\n" name (Energy.toFloat energy)
        | Model(brp) -> return! loop brp
        | _ -> 
            syncContext.RaiseEvent error <| Exception("BRP: Not implemented yet")

        return! loop brp
    }

    loop brp)