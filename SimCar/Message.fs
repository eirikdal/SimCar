module Message

open Agent
open Models

type Message = 
    | Charge of Agent<Message> * intent
    | Register of Agent<Message>
    | Deregister of Agent<Message>