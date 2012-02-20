module Agent

type Agent_Type = 
    | PHEV_Agent
    | Trf_Agent
    | BRP_Agent

type Agent<'T> = MailboxProcessor<'T>