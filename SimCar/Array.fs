module Array

let sum2 (list1 : float<'T> array) (list2 : float<'T> array) = list1 |> Array.map2 (fun sum t -> sum+t) list2
