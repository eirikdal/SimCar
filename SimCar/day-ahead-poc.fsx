let sine n = sin 1.0 * (float n)

let D = Array.init (20) sine
let x = Array.average D
let alpha = 0.9

let delta w' = alpha * (x - w')
let dist w' = sqrt ((x - w') ** 2.0)

let w = Array.maxBy (fun w' -> dist w') D
let i = Array.findIndex (fun w' -> w = w') D

let mutable k = i + 1
let mutable S = 0.0

let rec scan k = 
    let ik = i + k
    let ik' = i - k 

    if S > 0.0 then
        if ik <= D.Length then
            S <- S - delta D.[ik]
        if ik' >= 0 then
            S <- S - delta D.[ik']

        scan (k+1)

//let reduce k op neg = 
//    let ik = i+k
//    let ik' = i-k
//
//    if ik > 0 && w > D.[ik] then
//        S <- op S (delta D.[ik])
//        D.[ik] <- neg D.[ik] (delta D.[ik])
//    if ik' > 0 && w < D.[ik'] then 
//        S <- op S (delta D.[ik'])
//        D.[ik'] <- neg D.[ik'] (delta D.[ik'])        
//
//let rec build k = 
//    if S >= 0.0 then
//        
//        build (k+1)
//
//let rec scan k = 
//    if w < D.[i-k] && w < D.[i+k] then 
//        build (k+1)
//    else
//        if w > D.[i+k] || w > D.[i-k] then
//            reduce k (+) (-)
//        else if w < D.[k] then 
//            reduce k (-) (+)
//             
//        scan (k+1)