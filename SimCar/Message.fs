module Message

open System
open Agent
open Models

type Message = 
    | Charge of string * energy * int * energy
    | Charge_Received
    | Charge_OK of string
    | Charge_Accepted of energy
    | Charge_Intentions of string * Message list
    | Completed of string
    | Assign of Agent<Message> * Grid
    | Register of string * Agent<Message>
    | Deregister of string * Agent<Message>
    | Broadcast of Message
    | Dayahead of dayahead
    | Realtime of realtime
    | ReplyTo of Message * AsyncReplyChannel<Message>
    | RequestModel
    | Model of Grid
    | Error of string
    | Update of int
    | Reply of Message