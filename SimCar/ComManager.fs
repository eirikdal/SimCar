module ComManager

open System
open Agent
open Message

(* 
    ComManager: 

    This is the communication manager. It handles communication between agents.

    member self.send(from, to, msg): 
        This method takes a sender, recepient and a message
    member self.add_agent(agent, agent_type):
        This method takes an agent, and the agent type, and adds it to the list of agents that the ComManager is aware of 
    member self.remove_agent(agent, agent_type):
        Same as self.add_agent, except removes the agent from the list
*)
type ComManager() = 
    let agent = Agent.Start(fun agent ->
        let rec loop brp (phev_list : Agent<Message> list) trf_list = async {
            let! msg = agent.Receive()

            match msg with 
            | Register(agent, agent_type) ->
                match agent_type with
                | PHEV_Agent ->
                    return! loop brp (List.append [agent] phev_list) trf_list
                | _ -> raise (Exception("Could not find agent"))
            | Deregister(agent, agent_type) ->
                match agent_type with
                | PHEV_Agent -> 
                    return! loop brp (List.filter (fun ag -> ag <> agent) phev_list) trf_list
                | _ -> raise (Exception("Could not find agent"))
            | Hello(_from, _to) -> printfn "Hello %s" _from
            | _ -> ()
        }
        loop [] [] [])

    member self.send(msg) = 
        agent.Post(msg)

    member self.add_agent(agent : Agent<Message>, agent_type) = 
        agent.Post(Register(agent, agent_type))

    member self.remove_agent(agent : Agent<Message>, agent_type) = 
        agent.Post(Deregister(agent, agent_type))
    
//let phev_agents = 
//    list_of_phevs()
//    |> Seq.map (fun phev -> phev_agent phev)
//
//phev_agents
//    |> Seq.iter (fun phev -> sim_agent.Post(Register(phev, PHEV_Agent)))
//phev_agents
//    |> Seq.iter (fun phev -> phev.Post(Hello(phev.name,"")))
