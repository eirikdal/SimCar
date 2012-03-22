module Transformer

#nowarn "25"

open System
open System.Collections
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
    let queue = new Queue() 
    let rec loop (Transformer({ name=name; parent=parent; children=children } as trf_args) as trf) (intentions : Message list) = async {
        let! (msg : Message) = 
            if (intentions.Length >= children.Length && queue.Count > 0) then
                async { return queue.Dequeue() :?> Message }
            else
                agent.Receive()
        match msg with
        | Update(tick) ->
            return! loop trf intentions
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                if children.Length > intentions.Length then
                    queue.Enqueue(msg)
                    return! loop trf intentions
                else
                    reply.Reply(Model(trf))
                    return! loop trf []
        | Charge(from, energy, ttd, _) ->
            if (intentions.Length+1) = children.Length then
//                printfn "Charge from %s" from 
                postalService.send(parent, Charge_Intentions(name, (msg :: intentions)))
            return! loop trf (msg :: intentions)
        | Model(trf) -> 
            return! loop trf intentions
        | Charge_OK(from) as msg -> 
            if (intentions.Length+1) = children.Length then
//                printfn "Charge_OK from %s" from 
                postalService.send(parent, Charge_Intentions(name, (msg :: intentions)))
            return! loop trf (msg :: intentions)
        | Charge_Intentions(from,msgs) ->
            if (intentions.Length+1) = children.Length then
//                printfn "Charge_Intentions from %s" from 
                postalService.send(parent, Charge_Intentions(name, (msg :: intentions)))
            return! loop trf (msg :: intentions)
        | Reset ->
            return! loop trf intentions
        | _ as test ->
            raise (Exception((test.ToString())))
            return! loop trf intentions
    }
    loop trf [])
