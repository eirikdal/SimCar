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
        let rec loop brp (phev_list : Agent<Message> list) trf_list = async {
            let! msg = agent.Receive()

//            let find_recipient agent agent_type = 
//                match agent_type with
//                | PHEV_Agent ->
//                    phev_list |> Seq.find (fun ag -> agent = ag)
//                | _ -> raise (Exception("Not yet implemented!"))

            match msg with 
            | Register(from_agent, agent_type) ->
                match agent_type with
                | PHEV_Agent ->
                    printfn "Agent registered with postal service"
                    return! loop brp (List.append phev_list [from_agent]) trf_list
                | _ -> raise (Exception("Not yet implemented"))
            | Deregister(from_agent, agent_type) ->
                match agent_type with
                | PHEV_Agent ->
                    return! loop brp (List.filter (fun ag -> ag <> from_agent) phev_list) trf_list
                | _ -> raise (Exception("Not yet implemented"))
            | _ -> raise (Exception("Not yet implemented"))
        }
        loop [] [] [])

    member self.send_all(msg) = 
        match msg with 
        | _ -> raise (Exception("Not yet implemented"))

    member self.send(msg) = 
        match msg with
        | _ -> raise (Exception("Not yet implemented"))

    member self.add_agent(agent : Agent<Message>, agent_type) = 
        agent.Post(Register(agent, agent_type))

    member self.remove_agent(agent : Agent<Message>, agent_type) = 
        agent.Post(Deregister(agent, agent_type))