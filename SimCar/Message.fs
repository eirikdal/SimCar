module Message

open System
open Agent
open Models

type Message = 
    | Charge of string * energy
    | Completed of string
    | Assign of Agent<Message> * Grid
    | Register of string * Agent<Message>
    | Deregister of string * Agent<Message>
    | Broadcast of Message
    | Hello
    | ReplyTo of Message * AsyncReplyChannel<Message>
    | RequestModel
    | Model of Grid
    | Error of string
    | Update of int
    | Reply of Message