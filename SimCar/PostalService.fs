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
    let mutable _agents = Node(Seq.empty, None) : Node<Agent<_>>

    let agent = Agent<Message>.Start(fun agent ->
        let rec loop() = async {
            let! msg = agent.Receive()
            
            match msg with 
            | Register(name, from_agent) ->
                syncContext.RaiseEvent jobCompleted (agent, "Agent registered with postal service")

                agentdict.Add(name, from_agent)
            | Deregister(name, from_agent) ->
                syncContext.RaiseEvent jobCompleted (agent, "Agent deregistered from postal service")

                agentdict.Remove(name) |> ignore
//            | Broadcast(message) ->
//                agents |> List.iter (fun agent -> agent.Post(message))
            | Completed(message) ->
                printfn "%s" message
            | Error(message) ->
                printfn "Error: %s" message
            | _ ->
                syncContext.RaiseEvent error <| Exception("PostalService: Not implemented yet")

            return! loop()
        }
        loop())

    member self.Post(msg) = agent.Post(msg)

    member self.send(name, msg) = 
        agentdict.[name].Post(msg)

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