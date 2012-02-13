module Agent

type Agent_Type = 
    | PHEV_Agent
    | Transformer_Agent
    | BRP_Agent

type Agent<'T> = MailboxProcessor<'T>