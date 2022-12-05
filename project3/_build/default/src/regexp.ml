open List
open Nfa
open Sets

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let concatNfa (nfa1: (int, char) nfa_t) (nfa2: (int, char) nfa_t) : (int, char) nfa_t =
  (*joins sigma of nfa1 and 2, uses q0 of nfa1 as q0, creates epsilon
  transitions from fs of nfa1 to q0 of nfa2 uses fs of nfa2 as fs, 
  joins delta of nfa1 and 2, joins qs of nfa1 and 2*)
  let sigmaOut = insert_all nfa2.sigma nfa1.sigma in
  let deltaTemp = insert_all nfa2.delta nfa1.delta in
  let qsOut = insert_all nfa2.qs nfa1.qs in
  let joinTrans = List.fold_left(fun out final -> (final, None, nfa2.q0)::out) [] nfa1.fs in
  let deltaOut = insert_all joinTrans deltaTemp in
  {sigma = sigmaOut; q0 = nfa1.q0; qs = qsOut; fs = nfa2.fs; delta = deltaOut};;

let rec regexp_to_nfa (regex: regexp_t) : (int, char) nfa_t =
  match regex with
  |Empty_String -> let newF = fresh() in
                   let newS = fresh() in
    {sigma = []; q0 = newS; fs = [newF]; qs = [newS; newF]; delta = [(newS, None, newF)]}
  |Char c -> let newF = fresh() in
             let newS = fresh() in
    {sigma = [c]; q0 = newS; fs = [newF]; qs = [newS; newF]; delta = [(newS, Some c, newF)]}
  |Union (x, y) -> 
    let nfaX = regexp_to_nfa x in
    let nfaY = regexp_to_nfa y in
    let newF = fresh() in
    let newS = fresh() in
    let xFinalTrans = List.fold_left(fun trans xFinal -> (xFinal, None, newF)::trans) [] nfaX.fs in
    let yFinalTrans = List.fold_left(fun trans yFinal -> (yFinal, None, newF)::trans) [] nfaY.fs in
    let finalTranss = insert_all xFinalTrans yFinalTrans in
    let deltaComb = insert_all nfaX.delta nfaY.delta in
    {sigma = insert_all nfaX.sigma nfaY.sigma; q0 = newS; fs = [newF]; qs = insert_all [newS;newF] (insert_all nfaX.qs nfaY.qs); delta = insert_all [(newS, None, nfaX.q0); (newS, None, nfaY.q0)] (insert_all deltaComb finalTranss)}
  |Concat (a, b) ->
     concatNfa (regexp_to_nfa a) (regexp_to_nfa b)
  |Star reg -> 
    let n = regexp_to_nfa reg in
    let newF = fresh() in
    let newS = fresh() in
    let finalsToStart = List.fold_left(fun trans final -> (final, None, n.q0)::trans) [] n.fs in
    let finalsToEnd = List.fold_left(fun trans final -> (final, None, newF)::trans) [] n.fs in
    let deltaMinusStart = insert_all n.delta (insert_all finalsToStart finalsToEnd) in
    {sigma = n.sigma; q0 = newS; fs = [newF]; qs = insert_all n.qs [newS;newF]; delta = insert_all [(newS, None, n.q0); (newS, None, newF);] deltaMinusStart};;


(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
