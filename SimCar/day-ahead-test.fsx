#load "FSharpChart.fsx"
#load "day-ahead-shave.fsx" 

open MSDN.FSharp.Charting
open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions

let dist x x' = abs(x - x')
let mutable S = 0.0
let alpha = 0.3
let theta = 0.5

let shave(_D : float[]) = 
    let D = Array.copy _D

    S <- 0.0
    let x = Array.average D
    let w = Array.max D
    let i = Array.findIndex (fun w' -> w = w') D

    let disc idx = theta ** (dist (float idx) (float i))
    let delta w' idx (target : float) = (disc idx) * alpha * (target - w')
    let update idx target = 
        let d = (delta D.[idx] idx target)
        let S' = S + d
        
        D.[idx] <- D.[idx] + d
        S <- S'
    
    // target of peak value should be the average value
    update i x

    // continue shaving peaks toward mean while the value of the neighbornode is greater than the value of the updated node.
    // this is done to preserve topology
    let rec smooth k = 
        let ik = i + k
        let ik' = i - k

        if ik < D.Length && ik+1 < D.Length && ik'-1 > 0 && ik > 0 then
            if D.[ik] > D.[ik-1] then
                update ik x
            if D.[ik'] > D.[ik+1] then 
                update ik' x
            if D.[ik+1] > D.[ik] || D.[ik'-1] > D.[ik] then 
                smooth (k+1)

    smooth 1

    // distribute excess load
    let fill k target =
        let ik = i + k
        let ik' = i - k
            
        if ik < D.Length then
            update ik target
        if ik' > 0 then
            update ik' target

    // from peak value, distribute load while S < 0.0
    let rec _scan k = 
        if S < 0.0 && (i+k < D.Length || i-k > 0) then
            fill k D.[i]
            _scan (k+1)
        else if S < 0.0 then
            _scan 1

    _scan 1

    let dS = S / (2.0*(float D.Length+1.0))
    for i in 0 .. (D.Length-1) do
        D.[i] <- D.[i] - dS

    D

let folder_of file = sprintf "C:\\SimCar\\SimCar\\data\\%s" file

let read_file file = 
    seq {
        use sr = new StreamReader(folder_of file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let rec parse_powerprofile stream name (dist : float list) (rest : string list byref) = 
    match (stream : string list) with 
    | h::t ->
        match h.Split([|' ';';'|], StringSplitOptions.RemoveEmptyEntries) with
        | [|q1;q2;q3;q4|] ->
            let (q4',q3',q2',q1') = (Double.Parse(q4, CultureInfo.InvariantCulture),Double.Parse(q3, CultureInfo.InvariantCulture),Double.Parse(q2, CultureInfo.InvariantCulture),Double.Parse(q1, CultureInfo.InvariantCulture))
            parse_powerprofile t name (q4'::q3'::q2'::q1'::dist) (&rest)
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
            List.rev profiles
    | _ -> List.rev profiles

let powerprofiles : (string * float list) list = 
    let mutable rest = []
    let stream = List.ofSeq (read_file "powerprofiles.txt")

    parse_powerprofiles stream List.empty (&rest)

let mutable test = powerprofiles |> Seq.nth 0 |> snd |> Seq.take 96 |> Array.ofSeq
//
for i in 1 .. 10 do 
    test <- test |> shave
    System.Threading.Thread.Sleep(10)
    do FSharpChart.Line test
//    chart.SaveChartAs(sprintf "C:\\SimCar\\SimCar\\data\\%d.png" i, Windows.Forms.DataVisualization.Charting.ChartImageFormat.Png)