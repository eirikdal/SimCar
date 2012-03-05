module Message

open System
open Agent
open Models

type 'a Message = 
    | Charge of string * energy
    | Completed of string
    | Assign of Agent<'a Message> * Grid
    | Register of string * Agent<'a Message>
    | Deregister of string * Agent<'a Message>
    | Broadcast of 'a Message
    | Hello
    | ReplyTo of 'a Message * AsyncReplyChannel<'a Message>
    | RequestModel
    | Model of Grid
    | Error of string
    | Update of int
    | Reply of 'a