module DayAhead

open System
open Models

let dist x x' = abs(x - x')

let sine n = sin (2.0 * Math.PI * (float n))
let mutable S = 0.0<kWh>

let scan(_D : float<kWh>[]) = 
    let D = Array.copy _D

    S <- 0.0<kWh>
    let x = Array.average D
    let w = Array.max D
    let alpha = 0.2
    let i = Array.findIndex (fun w' -> w = w') D

    let disc idx = 0.95 ** (dist (float idx) (float i))
    let delta w' idx (target : float<kWh>) = (disc idx) * alpha * (target - w')
    let update idx target = 
        let d = (delta D.[idx] idx target)
        let S' = S + d
        
        D.[idx] <- D.[idx] + d
        S <- S'
    
    let nbhood = 3

    update i x
    
    let target = D.[i]

    let _scan nbhood =
        for k in -nbhood .. nbhood do
            let ik = i + k
            let ik' = i - k
            
            if ik < D.Length then
                update ik target
            if ik' > 0 then
                update ik' target

    while S < 0.0<kWh> do
        _scan (nbhood+1)

    let dS = S / (2.0*(float nbhood) + 1.0)
    for i in i - nbhood .. i + nbhood do
        D.[i] <- D.[i] - dS

    D