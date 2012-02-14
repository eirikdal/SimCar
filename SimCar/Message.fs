module Message

open Agent

type Message = 
    | Hello of string * string
    | Register of Agent<Message> * Agent_Type
    | Deregister of Agent<Message> * Agent_Type