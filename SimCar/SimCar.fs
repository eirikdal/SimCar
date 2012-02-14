// Learn more about F# at http://fsharp.net

open Agent
open System
open System.IO
open Models
open Message
open PHEV
open FileManager

[<EntryPoint>]
let main args = 
    let phevs = list_of_phevs()

    let sim_agent = Agent.Start(fun agent ->
        let rec loop brp (phev_list : Agent<Message> list) trf_list = async {
            let! msg = agent.Receive()

            match msg with 
            | Register(agent, agent_type) ->
                match agent_type with
                | PHEV_Agent ->
                    return! loop brp (List.append [agent] phev_list) trf_list
                | _ -> return! loop brp phev_list trf_list
            | Hello(_from, _to) -> printfn "Hello %s" _from
            | _ -> ()
        }
        loop [] [] [])

    let phev_agents = 
        list_of_phevs()
        |> Seq.map (fun phev -> phev_agent phev)

    phev_agents
        |> Seq.iter (fun phev -> sim_agent.Post(Register(phev, PHEV_Agent)))
    phev_agents
        |> Seq.iter (fun phev -> phev.Post(Hello(phev.name,"")))

    ignore(Console.ReadKey())
    0