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

//let parse_phevs = 
//    Seq.map (fun (str : string) -> 
//        match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with 
//        | [|name;capacity;current;battery|] -> 
//            PHEV(name,
//                None,
//                Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
//                Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture),
//                Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture))
//        | _ -> raise <| IOException("Error while reading PHEVs from file"))

let rec create_transformers trf_seq nodes (rest : string list byref) =
    match (trf_seq : string list) with 
    | h::t ->
        match h.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
        | [|"trf";name;capacity;current|] ->
            let node = 
                Node(
                    name,
                    Seq.empty,
                    Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
                    Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture))
            create_transformers t (List.append nodes [node]) (&rest)
        | [|"trf";name;capacity;current;"{"|] -> 
            let node = 
                Node(
                    name,
                    create_transformers t [] &rest,
                    Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
                    Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture))
            create_transformers rest (List.append nodes [node]) (&rest)
        | [|"phev";name;capacity;current;battery|] -> 
            let node = 
                PHEV(name,
                    Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
                    Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture),
                    Battery.ofFloat <| Double.Parse(battery, CultureInfo.InvariantCulture))
            create_transformers t (List.append nodes [node]) (&rest)
        | [|"pnode";name;capacity;current|] ->
            let node = 
                PowerNode("test", Capacity.ofFloat 0.0, Capacity.ofFloat 0.0)
            create_transformers t (List.append nodes [node]) (&rest)
        | [|"}"|] -> 
            rest <- t
            create_transformers t nodes (&rest)
        | _ -> raise <| IOException("Error while reading Transformers from file")
    | _ -> nodes

//let parse_trsf = 
//    Seq.map (fun (str : string) -> 
//        match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with 
//        | [|name;nodes;capacity;current|] -> 
//            Node(
//                name,
//                Seq.empty,
//                Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
//                Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture))
//        | [|name;capacity;current|] ->
//            Leaf(
//                name,
//                None,
//                None,
//                Capacity.ofFloat <| Double.Parse(capacity, CultureInfo.InvariantCulture),
//                Current.ofFloat <| Double.Parse(current, CultureInfo.InvariantCulture))
//
//        | _ -> raise <| IOException("Error while reading Transformers from file"))

//let list_of_phevs() = 
//    parse_phevs <| read_file "phevs.txt"
    
let list_of_trfs() = 
    let mutable rest = []
    let trfs = List.ofSeq (read_file "transformers.txt")
    create_transformers trfs [] (&rest)