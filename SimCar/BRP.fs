module BRP

#nowarn "25"

open Agent
open Message
open Models
open System
open System.Collections
open System.Threading
open SynchronizationContext

let brp_agent brp = Agent.Start(fun agent ->
    let queue = new Queue() 
    let rec loop (BRP({ children=children } as brp_args) as brp) (intentions : Message list) = async {
        let! (msg : Message) = 
            if (intentions.Length = children.Length && queue.Count > 0) then
                async { return queue.Dequeue() :?> Message }
            else
                agent.Receive()

        match msg with
        | ReplyTo(replyToMsg, reply) ->
            match replyToMsg with
            | RequestModel ->
                if children.Length > intentions.Length then
                    queue.Enqueue(msg)
                    return! loop brp intentions
                else
//                    printfn "BRP responding to RequestModel"
                    syncContext.RaiseEvent jobDebug <| "BRP responding to RequestModel"
                    reply.Reply(Model(brp))
                    return! loop brp []
        | Update(tick) -> 
            return! loop brp []
        | Dayahead(dayahead) ->
            return! loop <| BRP({ brp_args with dayahead=dayahead }) <| intentions
        | Realtime(realtime) ->
            return! loop <| BRP({ brp_args with realtime=realtime }) <| intentions
        | Model(brp) -> 
            return! loop brp intentions
        | Charge(from, energy, ttd) as msg ->
            syncContext.RaiseEvent jobDebug <| sprintf "%s received charge from %s" "BRP" from

            if intentions.Length + 1 >= children.Length then
                syncContext.RaiseEvent jobDebug <| "BRP got charges"
                return! loop brp (msg :: intentions)
            else
                return! loop brp (msg :: intentions)                
        | Charge_OK(from) as msg -> 
            syncContext.RaiseEvent jobDebug <| sprintf "%s received charge from %s" "BRP" from

            if intentions.Length + 1 >= children.Length then
                syncContext.RaiseEvent jobDebug <| "BRP got charges"
                return! loop brp (msg :: intentions)
            else                   
                return! loop brp (msg :: intentions)
        | _ -> 
            syncContext.RaiseEvent error <| Exception("BRP: Not implemented yet")

        return! loop brp intentions}    

    loop brp [])