module Message

open System
open Agent
open Models

type 'a Message = 
    | Charge of Agent<'a Message> * intent
    | Completed of string
    | Assign of Agent<'a Message> * Grid
    | Register of Agent<'a Message>
    | Deregister of Agent<'a Message>
    | Broadcast of 'a Message
    | Hello
    | ReplyTo of 'a Message * AsyncReplyChannel<'a Message>
    | RequestModel
    | Model of Grid
    | Error of string
    | Reply of 'a