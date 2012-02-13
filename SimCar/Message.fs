module Message

open Agent

type Message = 
    | Hello
    | Register of Agent<Message> * Agent_Type