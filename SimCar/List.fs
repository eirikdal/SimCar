module List

let map2 op list1 list2 = list1 |> List.map2 (fun sum t -> if (op sum t) < 1.0 then sum+t else 1.0) list2
let sum2 (list1 : float list) (list2 : float list) = list1 |> List.map2 (fun sum t -> sum+t) list2
let sumn list = list |> List.fold (fun ac dist -> sum2 ac dist) (List.init (96) (fun _ -> 0.0))
