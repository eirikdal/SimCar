
//#r "WindowsBase"
//#r "PresentationCore"
//#r "PresentationFramework"
//#r "WindowsFormsIntegration"
//#r "System.Xaml"
//#load "FSharpChart.fsx"
//open MSDN.FSharp

open System
open System.Windows

let dist x x' = sqrt ((x - x') ** 2.0)

let sine n = sin (2.0 * Math.PI * (float n))
let mutable S = 0.0
let mutable D = ([|for f in -0.5 .. 0.01 .. 1.0 -> if f < 0.0 || f > 0.5 || 0.1 >= sine f then 0.1 else sine f|])

let scan(D : float[]) = 
    S <- 0.0
    let x = Array.average D
    let w = Array.max D
    let alpha = 0.2
    let i = Array.findIndex (fun w' -> w = w') D

    let disc idx = 0.95 ** (dist (float idx) (float i))
    let delta w' idx (target : float) = (disc idx) * alpha * (target - w')
    let update idx target = 
        let d = (delta D.[idx] idx target)
        let S' = S + d
        
        D.[idx] <- D.[idx] + d
        S <- S'
    
    let nbhood = 3

    update i x
    
    let target = D.[i]

    let rec _scan k =
        let ik = i + k
        let ik' = i - k 
        
        if ik < (i+nbhood) && ik < D.Length then
            update ik target
        if ik' >= (i-nbhood) && ik' > 0 then
            update ik' target

        if ((ik < D.Length || ik' >= 0) && S < 0.0) then
            _scan (k+1)

    while S < 0.0 do
        _scan 1

    let dS = S / (2.0*(float nbhood) + 1.0)
    for i in i - nbhood .. i + nbhood do
        D.[i] <- D.[i] - dS

    D

D <- scan(D)

let area = Charting.FSharpChart.Area D
let host = new Forms.Integration.WindowsFormsHost(Child = new Charting.ChartControl(area))