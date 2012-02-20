module FileManager

open System
open System.IO
open System.Globalization
open Models

let folder_of file = sprintf "%s\\%s" (Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName) file

let read_file file = 
    seq {
        use sr = new StreamReader(folder_of file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let parse_phevs = 
    Seq.map (fun (str : string) -> 
        match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with 
        | [|name;capacity;current;battery|] -> 
            PHEV(name,
                None,
                Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
                Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture),
                Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture))
        | _ -> raise <| IOException("Error while reading PHEVs from file"))

let parse_trsf = 
    Seq.map (fun (str : string) -> 
        match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with 
        | [|name;nodes;capacity;current|] -> 
            Node(
                name,
                Seq.empty,
                Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
                Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture))
        | [|name;capacity;current|] ->
            Leaf(
                name,
                None,
                None,
                Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
                Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture))

        | _ -> raise <| IOException("Error while reading Transformers from file"))

let list_of_phevs() = 
    parse_phevs <| read_file "phevs.txt"

let list_of_trfs() = 
    parse_trsf <| read_file "transformers.txt"