module FileManager

open System
open System.IO
open Models

let read_file file = 
    seq {
        use sr = new StreamReader(sprintf "F:\Dokumenter\NTNU\TDT4900\SimCar\SimCar\%s.txt" file)
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
            Transformer_Node(
                Seq.empty,
                name,
                Capacity.ofFloat (Double.Parse(capacity, Globalization.CultureInfo.InvariantCulture)),
                Current.ofFloat (Double.Parse(current, Globalization.CultureInfo.InvariantCulture)))
        | [|name;capacity;current;bla;bla|] ->
            Transformer_Leaf()
        | _ -> raise <| System.IO.IOException("Error while reading PHEVs from file"))

let list_of_phevs() = 
    parse_phevs (read_file "phevs.txt")
    //    Seq.iter (fun x -> ()) phevs

let list_of_trsf() = 
    parse_trsf (read_file "transformers.txt")