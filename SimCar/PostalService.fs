module PostalService

#nowarn "25"

open System
open System.Threading
open SynchronizationContext
open Agent
open Message
open Models
open System.Collections.Generic
open Tree

//let syncContext = SynchronizationContext.CaptureCurrent()

(* 
    PostalService: 

    This is the postal service agent. It handles communication between agents.

    member self.send(from, to, msg): 
        This method takes a sender, recepient and a message
    member self.add_agent(agent, agent_type):
        This method takes an agent, and the agent type, and adds it to the list of agents that the postal service is aware of 
    member self.remove_agent(agent, agent_type):
        Same as self.add_agent, except removes the agent from the list
*)
type PostalService() = 
    let agentdict = new Dictionary<string, Agent<Message>>()
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
            | Kill -> printfn "PostalService: Exiting.."
            | Completed(message) ->
                printfn "%s" message

                return! loop()
            | Error(message) ->
                printfn "Error: %s" message
                return! loop()
            | _ ->
                syncContext.RaiseEvent error <| Exception("PostalService: Not implemented yet")

        }
        loop())

    member self.Post(msg) = agent.Post(msg)

    member self.send(name, msg) = 
        agentdict.[name].Post(msg)

    member self.send_reply(name, msg) = 
        agentdict.[name].PostAndReply(fun replyChannel -> ReplyTo(msg, replyChannel))

    member self.send_reply(name,msg,reply) = 
        agentdict.[name].Post(ReplyTo(msg, reply))

    member self.send_all(msg) = 
        _agents |> Tree.send RequestModel

    member self.send_to(to_agent : Agent<Message>, msg) =  
        to_agent.Post(msg)

    member self.agents with get() = _agents and set(agents : Node<string * Agent<Message>>) = _agents <- agents |> Tree.map (fun (name,ag) -> ag); Tree.iter self.add_agent agents

    member self.add_agent(name, from : Agent<Message>) = 
        agent.Post <| Register(name, from)

    member self.remove_agent(name, from : Agent<Message>) = 
        agent.Post <| Deregister(name, from)
//
//    member self.to_model(agent : Agent<Message>)= 
//        agent.PostAndReply(fun replyChannel -> Model(replyChannel))

let postalService = new PostalService()