module Message

#nowarn "25"

open System
open Agent
open Models

type Message = 
    | Charge of string * energy * int * energy
    | Charge_Received
    | Charge_OK of string * energy * int
    | Charge_Accepted of energy list
    | Charge_Intentions of Message list
    | Register of string * Agent<Message>
    | Deregister of string * Agent<Message>
    | Dayahead of dayahead
    | Prediction of realtime
    | ReplyTo of Message * AsyncReplyChannel<Message>
    | RequestMixed of string * int
    | RequestModel
    | RequestDayahead
    | Mixed of energy list
    | Model of Grid
    | Error of string
    | Update of int
    | Kill
    | Reset
    | Reply of Message
    | Schedule of (dayahead -> realtime -> (string * Message) list -> int -> unit)

let rec reduce_queue queue = 
    [for msg in queue do 
        match msg with 
        | Charge_Intentions(msg) ->
            yield! reduce_queue msg
        | Charge(name,_,_,_) -> yield! [name,msg]
        | Charge_OK(_) -> ()]