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
    let alpha = 0.15
    let i = Array.findIndex (fun w' -> w = w') D

    let disc idx = 0.8 ** (dist (float idx) (float i))
    let delta w' idx (target : float<kWh>) = (disc idx) * alpha * (target - w')
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
        if S < 0.0<kWh> && i+k < D.Length && i-k > 0 then
            fill k D.[i]
            _scan (k+1)
        else if S < 0.0<kWh> then
            _scan 1

    _scan 1

    let dS = S / (2.0*(float D.Length+1.0))
    for i in 0 .. (D.Length-1) do
        D.[i] <- D.[i] - dS

    D