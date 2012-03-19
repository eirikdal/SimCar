module FileManager 

#nowarn "25"

open Models
open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions

//let folder_of file = sprintf "%s\\%s" (Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName) file
let file_powerprofiles = "c:\\simcar\simcar\\data\\powerprofiles.txt"
let file_phevprofiles = "C:\\SimCar\\SimCar\\data\\profiles.txt"
let file_brp = "C:\\SimCar\\SimCar\\data\\brp.txt"
let file_dayahead = "c:\\simcar\simcar\\data\\dayahead.txt"
let file_prediction = "c:\\simcar\simcar\\data\\prediction.txt"

module IO =
    let read_file (file : string) = 
        if not <| File.Exists(file) then File.WriteAllText(file, "")
        seq {
    //        use sr = new StreamReader(folder_of file)
            use sr = new StreamReader(file)

            while not sr.EndOfStream do
                yield sr.ReadLine()
        }

    let write_to_file (file : string) (contents  : string seq) = 
        if File.Exists file_dayahead then
            File.AppendAllLines(file, contents)
        else
            File.WriteAllLines(file, contents)

    let clear_dayahead_data () = 
        File.Delete (file_dayahead)

module Regex = 
    let (|Integer|_|) (str: string) =
        let mutable intvalue = 0
        if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
        else None

    let (|Float|_|) (str: string) =
        let mutable floatvalue = 0.0
        if System.Double.TryParse(str, &floatvalue) then Some(floatvalue)
        else None

    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

module Parsing = 
    open Regex
    open IO 

    let parse_dayahead (_dayahead : float<kWh> list) = 
        let rec _parse dayahead ac =
            match dayahead with 
            | q4::q3::q2::q1::rest ->
                let (f1,f2,f3,f4) = (Energy.toFloat q1, Energy.toFloat q2, Energy.toFloat q3, Energy.toFloat q4)
                let str = sprintf "%f;%f;%f;%f" f1 f2 f3 f4
                _parse rest (str::ac) 
            | _ -> List.rev ac
        _parse _dayahead List.empty

    let parse_dist str = 
        match str with 
        | ParseRegex "mean=([0-9]+){1,2}:([0-9]+){1,2}" [Float h; Float m] ->
            h*4.0 + (m / 15.0)
        | ParseRegex "std=([0-9]+)" [Float std] ->
            (std / 15.0)
        | ParseRegex "duration=([0-9]+){1,2}:([0-9]+){1,2}" [Float h; Float m] ->
            h*4.0 + (m / 15.0)
        | _ -> raise <| Exception "Parsing failed"

    let rec parse_profile stream (dist : Distribution list) (rest : string list byref) = 
        match (stream : string list) with 
        | h::t ->
            match h.Split([|' ';','|], StringSplitOptions.RemoveEmptyEntries) with
            | [|dist_type;mean;std;duration|] ->
                let temp = create_distribution dist_type (parse_dist mean) (parse_dist std) (int <| parse_dist duration)
                parse_profile t (temp::dist) (&rest)
            | [|"}"|] -> 
                rest <- t
                dist
            | _ -> raise <| Exception "Unexpected end of stream."
        | _ -> raise <| Exception("Unexpected end of stream. Maybe missing closing '}'?")
            
    let rec parse_profiles stream profiles (rest : string list byref) =
        match (stream : string list) with 
        | h::t ->
            match h.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
            | [|profile;"{"|] ->
                let profile = DistProfile(profile, parse_profile t [] (&rest))
                parse_profiles rest (profile::profiles) (&rest)
            | _ -> 
                rest <- t
                profiles
        | _ -> profiles

    let profiles : Profile list = 
        let mutable rest = []
        let stream = List.ofSeq (read_file file_phevprofiles)

        parse_profiles stream [] (&rest)

    let rec parse_powerprofile stream name (dist : float list) (rest : string list byref) = 
        match (stream : string list) with 
        | h::t ->
            match h.Split([|' ';';'|], StringSplitOptions.RemoveEmptyEntries) with
            | [|q1;q2;q3;q4|] ->
                let temp = Double.Parse(q4, CultureInfo.InvariantCulture)::Double.Parse(q3, CultureInfo.InvariantCulture)::Double.Parse(q2, CultureInfo.InvariantCulture)::Double.Parse(q1, CultureInfo.InvariantCulture)::dist
                parse_powerprofile t name temp (&rest)
            | [|"}"|] -> 
                rest <- t
                List.rev dist
            | _ -> raise <| Exception "Unexpected end of stream."
        | _ -> raise <| Exception("Unexpected end of stream. Maybe missing closing '}'?")
     
    let rec parse_powerprofiles stream profiles (rest : string list byref) =
        match (stream : string list) with 
        | h::t ->
            match h.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
            | [|name;"{"|] ->
                let profile = (name, parse_powerprofile t name List.empty (&rest))
                parse_powerprofiles rest (profile::profiles) (&rest)
            | _ -> 
                rest <- t
                profiles
        | _ -> profiles

    let powerprofiles : (string * float list) list = 
        let mutable rest = []
    //    let stream = List.ofSeq (read_file "powerprofiles.txt")
        let stream = List.ofSeq (read_file file_powerprofiles)
        parse_powerprofiles stream List.empty (&rest)

    // 
    // Parsing the powergrid, transformers, power nodes and PHEVs.
    //
    let rec parse_powergrid stream nodes (rest : string list byref) (children : string list byref) parent =
        match (stream : string list) with 
        | h::t ->
            match h.Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) with
            | [|"trf";name;capacity;current|] ->
                let node = create_node name List.empty capacity current parent children
                children <- name :: children
                parse_powergrid t (node::nodes) (&rest) (&children) parent
            | [|"trf";name;capacity;current;"{"|] ->
                let mutable temp = []
                let node = create_node name (parse_powergrid t [] &rest (&temp) name) capacity current parent temp
                children <- name :: children
                parse_powergrid rest (node::nodes) (&rest) (&children) parent
            | [|"phev";name;profile;capacity;current;battery;rate|] -> 
                let node = create_phev name capacity current battery rate profile parent profiles
                children <- name :: children
                parse_powergrid t (node::nodes) (&rest) (&children) parent
            | [|"pnode";name;realtime|] ->
                let realtime = (List.tryFind (fun (n, s) -> n = realtime) powerprofiles)
                match realtime with
                | None -> raise <| IOException(sprintf "Could not find powernode with name %s in powerprofiles.txt" name)
                | Some realtime ->
                    let nth n = Energy.ofFloat ((snd realtime) |> Seq.cache |> Seq.nth n)
                    let node = create_powernode name nth parent
                    children <- name :: children
                    parse_powergrid t (node::nodes) (&rest) (&children) parent
            | [|"}"|] -> 
                rest <- t
                nodes
            | _ -> raise <| IOException("Error while reading Transformers from file")
        | _ -> nodes

    let rec parse_dayahead_file (ac : float<kWh> list) stream = 
        match (stream : string list) with 
        | h::t ->
            match h.Split([|' ';';'|], StringSplitOptions.RemoveEmptyEntries) with
            | [|q1;q2;q3;q4|] ->
                let (q1'::q2'::q3'::q4'::_) = 
                    q1::q2::q3::q4::[]
                    |> List.map (fun q -> Energy.ofFloat <| Double.Parse(q,CultureInfo.InvariantCulture))

                parse_dayahead_file (q1'::q2'::q3'::q4'::ac) t
            | _ -> raise <| Exception "Unexpected line"
        | _ -> List.rev ac
    
let dayahead() = List.ofSeq (IO.read_file file_dayahead) |> Parsing.parse_dayahead_file [] |> List.nth

let prediction() =  List.ofSeq (IO.read_file file_prediction) |> Parsing.parse_dayahead_file [] |> List.nth

let powergrid = 
    let mutable rest = []
    let mutable children : string list = []
    let stream = List.ofSeq (IO.read_file file_brp)

    create_brp "brp" (Parsing.parse_powergrid stream [] (&rest) (&children) "brp") (fun n -> 0.0<kWh>) (children)