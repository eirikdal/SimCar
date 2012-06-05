module Message

#nowarn "25"

open System
open Agent
open Models

type Message = 
    | Charge of string * energy * int * energy
    | Demand of string * energy * int
    | Register of string * Agent<Message>
    | Deregister of string * Agent<Message>
    | Dayahead of dayahead
    | Prediction of realtime
    | ReplyTo of Message * AsyncReplyChannel<Message>
    | RequestPredictions of string * int
    | RequestModel
    | RequestDayahead
    | Predictions of energy list
    | Model of Grid
    | Strategy of energy list
    | Error of string
    | Filter of bool
    | Update of int
    | Kill
    | Reset
    | Reply of Message
    | Schedule of schedule
and 
    schedule = (dayahead -> realtime -> Message list -> int -> unit)


//let rec reduce_queue queue = 
//    [for msg in queue do 
//        match msg with 
//        | Intentions(msg) ->
//            yield! reduce_queue msg
//        | Charge(name,_,_,_) -> yield! [name,msg]
//        | Demand(_) -> ()]