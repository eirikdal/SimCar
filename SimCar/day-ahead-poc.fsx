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

let scan(D) = 
    S <- 0.0
    let x = Array.average D
    let w = Array.max D
    let alpha = 0.2
    let i = Array.findIndex (fun w' -> w = w') D

    printfn "Area under D before algorithm: %f" (Array.sum D)
    printfn "Max: %f" w
//
//    let disc idx = 0.9 ** (float idx - float i)
//    let delta w' idx = (disc idx) * alpha * (x - w')
//    let update delta idx = 
//        S <- S + delta
//        D.[idx] <- D.[idx] + delta
//    let update w = delta w >> update
    let disc idx = 1.0 ** (dist (float idx) (float i))
    let delta w' idx = (disc idx) * alpha * (x - w')
    let update idx = 
        let delta = (delta D.[idx] idx)
        S <- S + delta
        D.[idx] <- D.[idx] + delta
    
    update i

    let rec _scan k =
        let ik = i + k
        let ik' = i - k 
        
        if ik < D.Length then
            update ik
        if ik' >= 0 then
            update ik'

        printfn "S: %f, ik: %i" S ik
        
        if ((ik < D.Length || ik' >= 0) && S < 0.0) then
            _scan (k+1)

    _scan 1

    printfn "Area under D after algorithm: %f" (Array.sum D)

    D

D <- scan(D)

let area = Charting.FSharpChart.Area D
let host = new Forms.Integration.WindowsFormsHost(Child = new Charting.ChartControl(area))