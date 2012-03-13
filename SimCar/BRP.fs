module BRP

#nowarn "25"

open Agent
open Message
open Models
open System
open System.Threading
open SynchronizationContext

let brp_agent brp = Agent.Start(fun agent ->
    let rec loop (BRP(brp_args) as brp) = async {
        let! msg = agent.Receive()

        match msg with
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(brp))
            | Charge(name, energy, ttd) -> 
                reply.Reply(Charge_Received)
        | Update(tick) -> 
            return! loop brp
//            return! collect_intentions brp []
        | Dayahead(dayahead) ->
            return! loop <| BRP({ brp_args with dayahead=dayahead })
        | Realtime(realtime) ->
            return! loop <| BRP({ brp_args with realtime=realtime })
        | Model(brp) -> 
            return! loop brp
        | _ -> 
            syncContext.RaiseEvent error <| Exception("BRP: Not implemented yet")

        return! loop brp }
    and collect_intentions (BRP({ children=children } as brp_args)) (intentions : Message list)
        = agent.Scan(function         
            | Charge(name, energy, ttd) as msg ->
                if intentions.Length + 1 = children.Length then
                    Some(async { return! loop brp })
                else
                    Some(async { return! collect_intentions brp (msg :: intentions) })
            | Charge_OK as msg -> 
                if intentions.Length + 1 = children.Length then
                    Some(async { return! loop brp })
                else
                    Some(async { return! collect_intentions brp (msg :: intentions) })
            | _ -> None)

    loop brp)