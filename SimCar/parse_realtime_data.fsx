﻿#r "bin/Debug/SimCar.dll"
#r "C:\SimCar\packages\MathNet.Numerics.2.1.2\lib\Net40\MathNet.Numerics.dll"
#r "C:\SimCar\packages\MathNet.Numerics.FSharp.2.1.2\lib\Net40\MathNet.Numerics.FSharp.dll"

open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions
open MathNet
open MathNet.Numerics.Interpolation
open FileManager

#nowarn "25"

let powerprofile_file = "C:\\SimCar\\SimCar\\data\\buskerud.txt"
let profile_file = "C:\\SimCar\\SimCar\\data\\powerprofiles.txt"
let data_folder = "C:\\SimCar\\SimCar\\data\\interpol\\"

let read_file = 
    seq {
        use sr = new StreamReader(powerprofile_file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }
//
let make_day' values = 
    [for qi in values do 
        for i in 1 .. 4 do yield Double.Parse(qi, NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture)]

let make_day values = 
    let v = [for qi in values do yield Double.Parse(qi, NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture)] |> Array.ofList
//    let v = make_day' values |> Array.ofList
    let test = [for i in 0 .. 4 .. (4*(v.Length-1)) do yield float i] |> Array.ofList
//    printfn "%d %d" v.Length test.Length
    let test = Algorithms.AkimaSplineInterpolation(test,v)
//    let test = Interpolate.LinearBetweenPoints(test, v)
    
    [for i in 0 .. 95 do yield test.Interpolate(float i)]

let rec parse_powerprofiles stream (values : float list) initdate days customer =
    match (stream : string list) with 
    | h::t ->
        match h.Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) with
        | [|cust;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_|] when cust = "Customer" ->
            parse_powerprofiles t values initdate days customer
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when days <= 0 && cust = customer ->
//            IO.write_doubles (data_folder + (sprintf "%s.dat" cust)) (List.rev values)
            parse_powerprofiles t values initdate (days-1) cust
        | [|cust;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_|] when days < 0 && customer = cust ->
            parse_powerprofiles t values initdate days cust
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when days <= 0 && cust <> customer ->
//            let hours = q1::q2::q3::q4::q5::q6::q7::q8::q9::q10::q11::q12::q13::q14::q15::q16::q17::q18::q19::q20::q21::q22::q23::q24::[]
//            let (day : float list) = [for f in make_day hours do yield f]
            IO.write_doubles (data_folder + (sprintf "%s.dat" customer)) (List.rev values)
            parse_powerprofiles (h::t) [] date 30 cust
        | [|cust;date;est;q1;q2;q3;q4;q5;q6;q7;q8;q9;q10;q11;q12;q13;q14;q15;q16;q17;q18;q19;q20;q21;q22;q23;q24|] when cust <> "Customer" ->
            let hours = q1::q2::q3::q4::q5::q6::q7::q8::q9::q10::q11::q12::q13::q14::q15::q16::q17::q18::q19::q20::q21::q22::q23::q24::[]
            let (day : float list) = make_day hours
            parse_powerprofiles t ((List.rev day) @ values) initdate (days-1) customer
    | _ -> ()

let powerprofiles = 
//    File.Delete(profile_file)
    for file in Directory.GetFiles(data_folder) do
        File.Delete(file)
    let mutable rest = []
    let stream = List.ofSeq read_file
    
    parse_powerprofiles stream [] ((new DateTime(2006,01,01)).ToString()) 30 "-1"
