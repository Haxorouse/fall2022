open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

exception ParseError of string

let try_match (toks: token list) (tok: token) =
  match toks with
  | [] -> []
  | h::t when h = tok -> t
  | h::t -> h::t

(*variable convention
  md<x> - (matched<x>) toks matched x times in current parse, ie list toks after x deterministic toks have been removed
  o<x> - (output<x>) output tupple from recursive calls to parse_expr
  ex<x> - (expression<x>) parsed expression pulled from o<x>*)
let rec parse_expr toks = 
  let typeIndicator = lookahead toks in
  if typeIndicator = Some Tok_Let then
    (*parse as let expression*)
    let md1 = match_token toks Tok_Let in
    if lookahead toks = Some Tok_Rec then
      let md2 = match_token md1 Tok_Rec in
      match (lookahead md2) with
      | Some Tok_ID name -> (
        let md3 = match_token md2 (Tok_ID name) in
        let md4 = match_token md3 Tok_Equal in
        let o1 = parse_expr md4 in
        match o1 with (md5, e1) ->
        let md6 = match_token md5 Tok_In in
        let o2 = parse_expr md6 in
        match o2 with (md7, e2) ->
        (*md7 * let rec name = e1 in e2*)
        (md7, Let(name, true, e1, e2))
      )
      | _ -> raise (ParseError "parse let no ID")
    else
      match (lookahead md1) with
      | Some Tok_ID name -> (
        let md2 = match_token md1 (Tok_ID name) in
        let md3 = match_token md2 Tok_Equal in
        let o1 = parse_expr md3 in
        match o1 with (md4, e1) ->
        let md5 = match_token md4 Tok_In in
        let o2 = parse_expr md5 in
        match o2 with (md6, e2) ->
        (*md6* let name = e1 in e2*)
        (md6, Let(name, false, e1, e2))
      )
      | _ -> raise (ParseError "parse let no ID")
  else if typeIndicator = Some Tok_If then
    (*parse as if expression*)
    let md1 = match_token toks Tok_If in
    let o1 = parse_expr md1 in
    match o1 with (md2, e1) ->
    let md3 = match_token md2 Tok_Then in
    let o2 = parse_expr md3 in
    match o2 with (md4, e2) ->
    let md5 = match_token md4 Tok_Else in
    let o3 = parse_expr md5 in
    match o3 with (md6, e3) ->
    (*md6 * if e1 then e2 else e3*)
    (md6, If(e1, e2, e3))
  else if typeIndicator = Some Tok_Fun then
    (*parse as function expression*)
    let md1 = match_token toks Tok_Fun in
    match (lookahead md1) with
    | Some Tok_ID name -> (
      let md2 = match_token md1 (Tok_ID name) in
      let md3 = match_token md2 Tok_Arrow in
      let o1 = parse_expr md3 in
      match o1 with (md4, e1) ->
      (*md4 * fun name -> e1*)
      (md4, Fun(name, e1))
    )
    | _ -> raise (ParseError "parse fun no ID")
  (*else if typeIndicator = None then*)
    (*terminate, no more tokens*)
  else 
    (*parse as or expression*)
    parseOrExpr toks
and parseOrExpr toks = 
  let o1 = parseAndExpr toks in
  match o1 with (md1, e1) ->
  if lookahead md1 = Some Tok_Or then
    let md2 = match_token md1 Tok_Or in
    let o2 = parseOrExpr md2 in
    match o2 with (md3, e2) ->
    (*md3 * e1 || e2*)
    (md3, Binop(Or, e1, e2))
  else o1
and parseAndExpr toks =
  let o1 = parseEqualityExpr toks in
  match o1 with (md1, e1) ->
  if lookahead md1 = Some Tok_And then
    let md2 = match_token md1 Tok_And in
    let o2 = parseAndExpr md2 in
    match o2 with (md3, e2) ->
    (*md3 * e1 && e2*)
    (md3, Binop(And, e1, e2))
  else o1
and parseEqualityExpr toks =
  let o1 = parseRelationalExpr toks in
  match o1 with (md1, e1) ->
  if lookahead md1 = Some Tok_Equal then
    let md2 = match_token md1 Tok_Equal in
    let o2 = parseEqualityExpr md2 in
    match o2 with (md3, e2) ->
    (*md3 * e1 opp e2*)
    (md3, Binop(Equal, e1, e2))
  else if lookahead md1 = Some Tok_NotEqual then
    let md2 = match_token md1 Tok_NotEqual in
    let o2 = parseEqualityExpr md2 in
    match o2 with (md3, e2) ->
    (md3, Binop(NotEqual, e1, e2))
  else o1
and parseRelationalExpr toks =
  let o1 = parseAdditiveExpr toks in
  match o1 with (md1, e1) ->
  let opps = lookahead md1 in
  if opps = Some Tok_Greater then
    let md2 = match_token md1 Tok_Greater in
    let o2 = parseRelationalExpr md2 in
    match o2 with (md3, e2) ->
    (*md3 * e1 opp e2*)
    (md3, Binop(Greater, e1, e2))
  else if opps = Some Tok_LessEqual then
    let md2 = match_token md1 Tok_Less in
    let o2 = parseRelationalExpr md2 in
    match o2 with (md3, e2) ->
    (md3, Binop(Less, e1, e2))
  else if opps = Some Tok_GreaterEqual then
    let md2 = match_token md1 Tok_GreaterEqual in
    let o2 = parseRelationalExpr md2 in
    match o2 with (md3, e2) ->
    (md3, Binop(GreaterEqual, e1, e2))
  else if opps = Some Tok_Less then
    let md2 = match_token md1 Tok_LessEqual in
    let o2 = parseRelationalExpr md2 in
    match o2 with (md3, e2) ->
    (md3, Binop(LessEqual, e1, e2))
  else o1
and parseAdditiveExpr toks =
  let o1 = parseMultiplicativeExpr toks in
  match o1 with (md1, e1) ->
  if lookahead md1 = Some Tok_Add then
    let md2 = match_token md1 Tok_Add in
    let o2 = parseAdditiveExpr md2 in
    match o2 with (md3, e2) ->
    (*md3 * e1 opp e2*)
    (md3, Binop(Add, e1, e2))
  else if lookahead md1 = Some Tok_Sub then
    let md2 = match_token md1 Tok_Sub in
    let o2 = parseAdditiveExpr md2 in
    match o2 with (md3, e2) ->
    (md3, Binop(Sub, e1, e2))
  else o1
and parseMultiplicativeExpr toks =
  let o1 = parseConcatExpr toks in
  match o1 with (md1, e1) ->
  if lookahead md1 = Some Tok_Mult then
    let md2 = match_token md1 Tok_Mult in
    let o2 = parseMultiplicativeExpr md2 in
    match o2 with (md3, e2) ->
    (*md3 * e1 opp e2*)
    (md3, Binop(Mult, e1, e2))
  else if lookahead md1 = Some Tok_Div then
    let md2 = match_token md1 Tok_Div in
    let o2 = parseMultiplicativeExpr md2 in
    match o2 with (md3, e2) ->
    (md3, Binop(Div, e1, e2))
  else o1
and parseConcatExpr toks =
  let o1 = parseUnaryExpr toks in
  match o1 with (md1, e1) ->
  if (lookahead md1 = Some Tok_Concat) then
    let md2 = match_token md1 Tok_Concat in
    let o2 = parseConcatExpr md2 in
    match o2 with (md3, e2) ->
    (*md3 * e1 ^ e2*)
    (md3, Binop(Concat, e1, e2))
  else o1
and parseUnaryExpr toks =
  if lookahead toks = Some Tok_Not then
    let md1 = match_token toks Tok_Not in
    let o1 = parseUnaryExpr md1 in
    match o1 with (md2, e1) ->
    (*md2 * not e1*)
    (md2, Not(e1))
  else 
    parseFunctionCallExpr toks
and parseFunctionCallExpr toks =
  let o1 = parsePrimaryExpr toks in
  match o1 with (md1, e1) -> 
  let nextTok = lookahead md1 in
  match nextTok with
    | None -> o1
    | Some Tok_Int n -> (
      let o2 = parsePrimaryExpr md1 in
      match o2 with (md2, e2) -> (md2, FunctionCall(e1, e2)))
    | Some Tok_Bool b -> (
      let o2 = parsePrimaryExpr md1 in
      match o2 with (md2, e2) -> (md2, FunctionCall(e1, e2)))
    | Some Tok_String s -> (
      let o2 = parsePrimaryExpr md1 in
      match o2 with (md2, e2) -> (md2, FunctionCall(e1, e2)))
    | Some Tok_ID i -> (
      let o2 = parsePrimaryExpr md1 in
      match o2 with (md2, e2) -> (md2, FunctionCall(e1, e2)))
    | Some Tok_RParen -> (
      let o2 = parsePrimaryExpr md1 in
      match o2 with (md2, e2) -> (md2, FunctionCall(e1, e2)))
    | _ -> o1 
and parsePrimaryExpr toks =
  let valTok = lookahead toks in 
  match valTok with
    | Some Tok_Int number -> (
      let md1 = match_token toks (Tok_Int number) in
      (md1, Value(Int(number)))
    )
    | Some Tok_Bool boolean -> (
      let md1 = match_token toks (Tok_Bool boolean) in
      (md1, Value(Bool(boolean)))
    )
    | Some Tok_String word -> (
      let md1 = match_token toks (Tok_String word) in
      (md1, Value(String(word)))
    )
    | Some Tok_ID name -> (
      let md1 = match_token toks (Tok_ID name) in
      (md1, ID(name))
    )
    | Some Tok_RParen -> (
      let md1 = match_token toks Tok_RParen in
      let o1 = parse_expr md1 in
      match o1 with (md2, e1) ->
      let md3 = match_token md2 Tok_LParen in
      (md3, e1)
    )
    | _ -> (
      raise (ParseError ("bad match:" ^ (string_of_list (fun a -> string_of_token a) toks))));;
(* Part 3: Parsing mutop *)

let rec parse_mutop toks = failwith "unimplemented"