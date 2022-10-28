open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e =
    let find = map (fun x -> if x = e then true else false) lst in
    fold (fun a b -> if a || b then true else false) false find;;

let is_present lst x =
    map (fun a -> if a = x then 1 else 0) lst;;

let count_occ lst target =
    fold (fun a elem -> if elem = target then a+1 else a) 0 lst;;

let uniq lst = 
    fold (fun out count -> if (count_occ out count) > 0 then out else count::out) [] lst;;

let assoc_list lst =
    let noDup = uniq lst in
    map (fun x -> x, (count_occ lst x)) noDup;;

let ap fns args = 
    fold (fun a f -> a@(map f args)) [] fns;;
