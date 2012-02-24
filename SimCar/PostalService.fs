module ComManager

open System
open System.Threading
open SynchronizationContext
open Agent
open Message

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
    let agent = Agent<'T Message>.Start(fun agent ->
        let rec loop agents = async {
            let! msg = agent.Receive()
            
            match msg with 
            | Register(from_agent) ->
                syncContext.RaiseEvent jobCompleted (agent, "Agent registered with postal service")
                return! loop <| List.append agents [from_agent]
            | Deregister(from_agent) ->
                syncContext.RaiseEvent jobCompleted (agent, "Agent deregistered from postal service")
                return! loop <| List.filter (fun ag -> ag <> from_agent) agents
            | Broadcast(message) ->
                agents |> List.iter (fun agent -> agent.Post(message))
            | Completed(message) ->
                printfn "%s" message
            | Error(message) ->
                printfn "Error: %s" message
            | _ ->
                syncContext.RaiseEvent error <| Exception("Not yet implemented")

            return! loop agents
        }
        loop [])

    member self.Post(msg) = agent.Post(msg)

    member self.send_to_all(msg) = 
        agent.Post(Broadcast(msg))

    member self.send_to(to_agent : Agent<unit Message>, msg) = 
        to_agent.Post(msg)

    member self.add_agent(from : Agent<'T Message>) = 
        agent.Post <| Register(from)

    member self.remove_agent(from : Agent<'T Message>) = 
        agent.Post <| Deregister(from)
//
//    member self.to_model(agent : Agent<Message>)= 
//        agent.PostAndReply(fun replyChannel -> Model(replyChannel))