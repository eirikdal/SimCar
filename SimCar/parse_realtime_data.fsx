open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions

let folder_of file = sprintf "%s\\data\\%s" (Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName) file
let profile_file = sprintf "%s\\%s" (Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName) "powerprofiles.txt"

let read_file file = 
    seq {
        use sr = new StreamReader(folder_of file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let make_day values = 
    List.map (fun qi -> 
                let f = Double.Parse(qi, NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) / 4.0
                sprintf "%f;%f;%f;%f" f f f f) values

let rec parse_powerprofiles stream (values : string list) days =
    match (stream : string list) with 
    | h::t ->
        match h.Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) with
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when days = 0 ->
            File.WriteAllLines (profile_file, ["house {"] @ (List.rev values) @ ["}"]) 
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when cust <> "Customer" ->
            let hours = q1::q2::q3::q4::q5::q6::q7::q8::q9::q10::q11::q12::q13::q14::q15::q16::q17::q18::q19::q20::q21::q22::q23::q24::[]
            let (day : string list) = make_day hours
            parse_powerprofiles t ((List.rev day) @ values) (days-1)
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when cust = "Customer" ->
            parse_powerprofiles t values days
    | _ -> File.WriteAllLines (profile_file, ["house {"] @ values @ ["}"])

let powerprofiles = 
    let mutable rest = []
    let stream = List.ofSeq (read_file "buskerud.txt")

    parse_powerprofiles stream [] 30
