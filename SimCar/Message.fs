module Message

open Agent
open Models

type Message = 
    | Charge of Agent<Message> * intent
    | Assign of Agent<Message> * Type
    | Register of Agent<Message>
    | Deregister of Agent<Message>
    | Broadcast of Message
    | Hello
and Type = 
    | MSG_PHEV of PHEV
    | MSG_Transformer of Transformer