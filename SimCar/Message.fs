module Message

open Agent
open Models

type Message = 
    | Charge of Agent<Message> * intent
    | Completed of string
    | Assign of Agent<Message> * Type
    | Register of Agent<Message>
    | Deregister of Agent<Message>
    | Broadcast of Message
    | Hello
and Type = 
    | PHEV of Node
    | Transformer of Node