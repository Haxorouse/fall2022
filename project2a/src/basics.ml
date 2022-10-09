(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
    match tup with 
    (a, b, c) -> (c, b, a);;

let is_odd x =
    if (x mod 2) = 0 then false else true;;

let area x y = 
match x with (a, b) ->
match y with (c, d) ->
    abs((a-(c)) * (b-(d)));;

let volume x y = 
match x with (a, b, c) ->
match y with (d, e, f) ->
    abs((a-(d)) * (b-(e)) * (c-(f)));;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =
    match n with
    0 -> 0
    | 1 -> 1
    | _ ->
    (fibonacci (n-1)) + (fibonacci (n-2));;

let rec pow x y =
    match y with
    0 -> 1
    | _ ->
    x * (pow x (y-1));;

let rec log x y = 
    if(y/x) < 1 then 0
    else 1 + (log x (y/x));;

let rec gcf x y = 
    if x = 0 then y
    else gcf (y mod x) x;;

let rec is_prime_recurse x d =
    if (x/d) < d then true
    else if (x mod d) = 0 then false
    else is_prime_recurse x (d+1);;

let rec is_prime x =
    match x with
    0 -> false
    | 1 -> false
    | _ -> if x < 0 then false
    else is_prime_recurse x 2;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
    if idx > 0 then
        match lst with
        | [] -> failwith "Out of bounds"
        | _::l -> get (idx-1) l
    else match lst with
        | [] -> failwith "Out of bounds"
        | i::_ -> i;;

let rec size lst a =
    match lst with
    | [] -> a
    | _::t -> size t (a+1);;

let larger lst1 lst2 = 
    let diff = (size lst1 0) - (size lst2 0) in
    if diff = 0 then []
    else if diff > 0 then lst1
    else lst2;;

let rec reverse_recurse lst a =
    match lst with
    | [] -> a
    | h::t -> reverse_recurse t (h::a);;

let reverse lst =
    reverse_recurse lst [];;

let rec combine lst1 lst2 =
    let lstw = reverse lst1 in
    match lstw with
    | [] -> lst2
    | h::t -> combine t (h::lst2);;

let rec merge_recurse lst1r lst2r a =
    match lst1r with
    | [] -> (match lst2r with
        | [] -> a
        | h2::t2 -> merge_recurse lst1r t2 (h2::a))
    | h1::t1 -> (match lst2r with
        | [] -> merge_recurse t1 lst2r (h1::a)
        | h2::t2 -> if h1 = h2 then merge_recurse t1 t2 (h1::a)
            else if h1 > h2 then merge_recurse t1 lst2r (h1::a)
            else merge_recurse lst1r t2 (h2::a));;

let rec merge lst1 lst2 = 
    let lstr1 = reverse lst1 in
    let lstr2 = reverse lst2 in
    merge_recurse lstr1 lstr2 [];;

let rec rotate shift lst =
    if shift > 0 then
    match lst with 
    | [] -> []
    | h::t -> (
        rotate (shift - 1) (combine t (h::[]))
    )
    else lst;;

let rec is_palindrome lst = 
    match lst with
    | [] -> true
    | x::[] -> true
    | h::t -> (
        let rev = reverse t in
        match rev with
        | [] -> false
        | e::mid -> (
            if h = e then is_palindrome mid
            else false
        )
    );;