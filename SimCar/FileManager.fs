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
let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"

module IO =
    let read_file (file : string) = 
        if not <| File.Exists(file) then File.WriteAllText(file, "")
    //        use sr = new StreamReader(folder_of file)
        use sr = new StreamReader(file)

        [while not sr.EndOfStream do
            yield sr.ReadLine()]

    let write_doubles (file : string) (contents : float list) = 
        use bw = new BinaryWriter(File.Open(file, FileMode.Append))
            
        contents |> List.iter (fun q -> bw.Write(q)) |> ignore

    let read_doubles (file : string) = 
        use br = new BinaryReader(File.Open(file, FileMode.Open))

        let length = int br.BaseStream.Length
        
        let bytes = br.ReadBytes(length)
        let doubles = Array.init (length / sizeof<float>) (fun _ -> 0.0)
        Buffer.BlockCopy(bytes, 0, doubles, 0, length)
        doubles

    let write_to_file (file : string) (contents  : string seq) = 
        if File.Exists file_dayahead then
            File.AppendAllLines(file, contents)
        else
            File.WriteAllLines(file, contents)

    let clear_dayahead_data () = 
        File.Delete (file_prediction)
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

    let parse_dayahead (_dayahead : float<kWh> list) = List.map (fun f -> Energy.toFloat f) _dayahead

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
        let stream = read_file file_phevprofiles

        parse_profiles stream [] (&rest)

    let parse_powerprofiles () =
        let files = Directory.GetFiles(data_folder)

        files 
        |> Array.map (fun file -> 
            match file with
            | Regex.ParseRegex "([0-9]+).dat" [Integer i] ->
                let name = sprintf "%i" i
                (name, read_doubles(file)))
        |> List.ofArray

    
    let powerprofiles = parse_powerprofiles ()
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
                    let nth = (snd realtime) |> Array.get >> Energy.ofFloat
                    let node = create_powernode name nth parent
                    children <- name :: children
                    parse_powergrid t (node::nodes) (&rest) (&children) parent
            | [|"}"|] -> 
                rest <- t
                nodes
            | _ -> raise <| IOException("Error while reading Transformers from file")
        | _ -> nodes

    let parse_dayahead_file (file) = read_doubles(file)
    
let dayahead() = Parsing.parse_dayahead_file(file_dayahead) |> Array.get >> Energy.ofFloat

let prediction() = Parsing.parse_dayahead_file(file_prediction) |> Array.get >> Energy.ofFloat

let powergrid = 
    let mutable rest = []
    let mutable children : string list = []
    let stream = IO.read_file file_brp

    create_brp "brp" (Parsing.parse_powergrid stream [] (&rest) (&children) "brp") (fun n -> 0.0<kWh>) (children)