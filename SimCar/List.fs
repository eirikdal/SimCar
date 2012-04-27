module List

let map2 op (list1 : float<'T> list) (list2 : float<'T> list) = list1 |> List.map2 (fun (sum : float<'T>) t -> if (op sum t) < 1.0 then sum+t else 1.0<_>) list2
let sum2 (list1 : float<'T> list) (list2 : float<'T> list) = list1 |> List.map2 (fun sum t -> sum+t) list2
let sumn (list : float<'T> list list) : float<'T> list = list |> List.fold (fun ac dist -> sum2 ac dist) (List.init (96) (fun _ -> 0.0<_>))
let avgn lists = 
    let n = lists |> List.length
    let lists_sum = sumn lists
    List.map (fun x -> x / (float n)) lists_sum