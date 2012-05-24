module PostalService

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Agent
open Message
open Models
open System.Collections.Generic

type PostalService() = 
    let mutable agentdict = new Dictionary<string, Agent<Message>>()
    let mutable _agents = Node(List.empty, None) : Node<Agent<_>>

    let agent = Agent<Message>.Start(fun agent ->
        let rec loop() = async {
            let! msg = agent.Receive()
            
            match msg with 
            | Register(name, from_agent) ->
                agentdict.Add(name, from_agent)
                return! loop()
            | Deregister(name, from_agent) ->
                agentdict.Remove(name) |> ignore
                return! loop()
            | Kill -> syncContext.RaiseDelegateEvent jobProgress <|  "PostalService: Exiting.."
            | Error(message) ->
                syncContext.RaiseDelegateEvent jobProgress <| sprintf "Error: %s" message
                return! loop()
            | _ ->
                syncContext.RaiseDelegateEvent jobError "PostalService: Not implemented yet"

        }
        loop())

    member self.Reset() = agentdict <- new Dictionary<string, Agent<Message>>()

    member self.Post(msg) = agent.Post(msg)

    member self.send(name, msg) = 
        agentdict.[name].Post(msg)

    member self.send_reply(name, msg) = 
        agentdict.[name].PostAndReply(fun replyChannel -> ReplyTo(msg, replyChannel))

    member self.send_reply(name,msg,reply) = 
        agentdict.[name].Post(ReplyTo(msg, reply))

    member self.send_to(to_agent : Agent<Message>, msg) =  
        to_agent.Post(msg)

    member self.add_agent(name, from : Agent<Message>) = 
        agent.Post <| Register(name, from)
    
    member self.remove_agent(name, from : Agent<Message>) = 
        agent.Post <| Deregister(name, from)

let postalService = new PostalService()