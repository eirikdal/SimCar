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
    let rec loop (Transformer({ parent=parent; children=children } as trf_args) as trf) = async {
        let! msg = agent.Receive()
        
        match msg with
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                reply.Reply(Model(trf))
                return! loop trf
            | Charge(name, energy, ttd) as intention -> 
                let replyInt = postalService.send_reply(parent, intention)
                reply.Reply(replyInt)
                return! loop trf
        | Model(trf) -> 
            return! loop trf
        | Update(tick) ->
            if children.Length > 0 then
                return! collect_intentions trf []
            else
                postalService.send(parent, Charge_OK)
                return! loop trf
        | _ -> 
            syncContext.RaiseEvent error <| Exception("Trf: Not yet implemented")

            return! loop trf
        } 
    and collect_intentions (Transformer({ parent=parent; children=children } as trf_args) as trf) (intentions : Message list) = agent.Scan((function         
        | Charge(name, energy, ttd) as msg ->
            if intentions.Length > 0 then
                //postalService.send(parent, Charge_OK)
                printfn "test"
                Some(async { return! loop trf })
            else    
                printfn "test"
                Some(collect_intentions trf (msg :: intentions))
        | Charge_OK as msg -> 
            if intentions.Length > 0 then
//                    postalService.send(parent, Charge_OK)
                printfn "test"
                Some(async { return! loop trf })
            else
                printfn "test"
                Some(collect_intentions trf (msg :: intentions))
        | _ ->
            None), 1000)
    
    loop trf)
