module Message

open Agent

type Message = 
    | Register of Agent<Message> * Agent_Type
    | Deregister of Agent<Message> * Agent_Type