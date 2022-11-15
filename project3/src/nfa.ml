open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  (*process list of transitions recursively to build list of outputs*)
  let rec add (edges: ('q, 's) transition list) (qs: 'q list) (s: 's option) : 'q list =
    match edges with
    | [] -> []
    | x::xt ->
      (*check if x starts at a position in qs and moves with option s
      if so return result of x::add xt qs s, if not return []::add xt qs s*)
    match x with
    (start, m, ending) ->
    if elem start qs && m = s then
    insert ending (add xt qs s) else
    add xt qs s
  in
    add nfa.delta qs s;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec process ends =
    let newL = insert_all ends (move nfa ends None) in
    if eq newL ends then newL else process newL
  in
   process  qs;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let chars = explode s in
  let rec process cs poss = 
    match cs with
    | [] -> e_closure nfa poss
    | x::xt -> process xt (e_closure nfa (move nfa (e_closure nfa poss) (Some x)))(*may need conversion to option*)
    in
  let ends = process chars [nfa.q0] in
  let check = intersection ends nfa.fs in
  match check with
    | [] -> false
    | x::xt -> true;;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left(fun out c -> (e_closure nfa (move nfa qs (Some c)))::out) [] nfa.sigma;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let rec loopSig alpha =
    match alpha with
    | [] -> []
    | x::xt -> 
      (qs, Some x, (e_closure nfa (move nfa qs (Some x))))::loopSig xt
  in
  loopSig nfa.sigma;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (List.fold_left (fun out pos -> if elem pos nfa.fs then true else out) false qs) then [qs] else [];;


let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  (*run new states on each state in work, if any of those states aren't in
  dfa.qs then add them, generate the new transitions for any new states,
  recurse with work being any new states added(either run dif on the input
  and new built dfa or track all ststaes added)*)
  let newSates = List.fold_left (fun out state -> insert_all (new_states nfa state) out) dfa.qs work in
  let newTrans = List.fold_left (fun out state -> insert_all (new_trans nfa state) out) dfa.delta work in
  let newWork = (diff newSates dfa.qs) in
  if eq newWork [] then {sigma = dfa.sigma; q0 = dfa.q0; qs = newSates; delta = newTrans; fs = []} else
  nfa_to_dfa_step nfa {sigma = dfa.sigma; q0 = dfa.q0; qs = newSates; delta = newTrans; fs = []} newWork;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa = {sigma = nfa.sigma; q0 = [nfa.q0]; qs = [[nfa.q0]]; delta = []; fs = []} in
  (*use nfa to dfa step to build the dfa then work out the final states
  and return*)
  let dfa2 = nfa_to_dfa_step nfa dfa [dfa.q0] in
  let finals = List.fold_left (fun out state -> insert_all out (new_finals nfa state)) [] dfa2.qs in
  {sigma = dfa2.sigma; q0 = dfa2.q0; qs = dfa2.qs; delta = dfa2.delta; fs = finals};;