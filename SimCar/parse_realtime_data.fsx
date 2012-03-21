#r "bin/Debug/SimCar.dll"

open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions
open FileManager


#nowarn "25"

let powerprofile_file = "C:\\SimCar\\SimCar\\data\\buskerud.txt"
let profile_file = "C:\\SimCar\\SimCar\\data\\powerprofiles.txt"
let data_folder = "C:\\SimCar\\SimCar\\data\\powernodes\\"

let read_file = 
    seq {
        use sr = new StreamReader(powerprofile_file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let make_day values = 
    [for qi in values do 
        for i in 1 .. 4 do yield Double.Parse(qi, NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) / 4.0]

let rec parse_powerprofiles stream (values : float list) days customer =
    match (stream : string list) with 
    | h::t ->
        match h.Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) with
        | [|cust;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_|] when cust = "Customer" ->
            parse_powerprofiles t values days customer
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when days = 0 ->
            IO.write_doubles (data_folder + (sprintf "%s.dat" cust)) (List.rev values)
            parse_powerprofiles t [] (days-1) cust
        | [|cust;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_|] when days < 0 && customer = cust ->
            parse_powerprofiles t values days cust
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when days < 0 && cust <> customer ->
            let hours = q1::q2::q3::q4::q5::q6::q7::q8::q9::q10::q11::q12::q13::q14::q15::q16::q17::q18::q19::q20::q21::q22::q23::q24::[]
            let (day : float list) = [for f in make_day hours do yield f]
            parse_powerprofiles t ((List.rev day) @ values) 30 cust
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when cust <> "Customer" ->
            let hours = q1::q2::q3::q4::q5::q6::q7::q8::q9::q10::q11::q12::q13::q14::q15::q16::q17::q18::q19::q20::q21::q22::q23::q24::[]
            let (day : float list) = make_day hours
            parse_powerprofiles t ((List.rev day) @ values) (days-1) customer
    | _ -> ()

let powerprofiles = 
    File.Delete(profile_file)
    let mutable rest = []
    let stream = List.ofSeq read_file

    parse_powerprofiles stream [] 30 "-1"
