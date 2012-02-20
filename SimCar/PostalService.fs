module ComManager

open System
open Agent
open Message

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
    let agent = Agent<Message>.Start(fun agent ->
        let rec loop agents = async {
            let! msg = agent.Receive()
            
            match msg with 
            | Register(from_agent) ->
                printfn "Agent registered with postal service"
                return! loop <| List.append agents [from_agent]
            | Deregister(from_agent) ->
                printfn "Agent deregistered from postal service"
                return! loop <| List.filter (fun ag -> ag <> from_agent) agents
            | Broadcast(message) ->
                agents |> List.iter (fun agent -> agent.Post(message))
            | _ -> failwith "Not yet implemented"

            return! loop agents
        }
        loop [])

    member self.send_all(msg) = 
        agent.Post(Broadcast(msg))

    member self.send(from_agent, to_agent, msg) = 
        match msg with
        | _ -> failwith "Not yet implemented"

    member self.add_agent(from : Agent<Message>) = 
        agent.Post <| Register(from)

    member self.remove_agent(from : Agent<Message>) = 
        agent.Post <| Deregister(from)