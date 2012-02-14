// Learn more about F# at http://fsharp.net

open Agent
open System
open System.IO
open Models
open Message
open PHEV


let read_file file = 
    seq {
        use sr = new StreamReader(Directory.GetParent(Environment.CurrentDirectory).Parent.FullName + "/" + file)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let parse_phevs = 
    Seq.map (fun (str : string) -> 
        match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with 
        | [|name;capacity;current;battery|] -> 
            PHEV(None,
                name,
                Capacity.ofFloat (Double.Parse(capacity, Globalization.CultureInfo.InvariantCulture)),
                Current.ofFloat (Double.Parse(current, Globalization.CultureInfo.InvariantCulture)),
                Battery.ofFloat (Double.Parse(battery, Globalization.CultureInfo.InvariantCulture)))
        | _ -> raise <| System.IO.IOException("Error while reading PHEVs from file"))

let parse_trsf = 
    Seq.map (fun (str : string) -> 
        match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with 
        | [|name;capacity;current|] -> 
            Transformer_Leaf(
                name, None, None,
                Capacity.ofFloat (Double.Parse(capacity, Globalization.CultureInfo.InvariantCulture)),
                Current.ofFloat (Double.Parse(current, Globalization.CultureInfo.InvariantCulture)))
        | [|name;capacity;current;bla;bla2|] ->
            Transformer_Leaf(
                name, None, None,
                Capacity.ofFloat (Double.Parse(capacity, Globalization.CultureInfo.InvariantCulture)),
                Current.ofFloat (Double.Parse(current, Globalization.CultureInfo.InvariantCulture)))
        | _ -> raise <| System.IO.IOException("Error while reading PHEVs from file"))

let list_of_phevs() = 
    parse_phevs (read_file "phevs.txt")
    //    Seq.iter (fun x -> ()) phevs

let list_of_trsf() = 
    parse_trsf (read_file "transformers.txt")

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

//    sim_agent.Post(Hello)
    ignore(Console.ReadKey())
    0