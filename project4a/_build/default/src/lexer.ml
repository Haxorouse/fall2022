open TokenTypes

(*copy of print functions from utils to avoid dependency cycle*)

    let string_of_token (t : token) : string = match t with
  | Tok_Sub -> "Tok_Sub"
  | Tok_RParen -> "Tok_RParen"
  | Tok_Add -> "Tok_Add"
  | Tok_Or -> "Tok_Or"
  | Tok_NotEqual -> "Tok_NotEqual"
  | Tok_Not -> "Tok_Not"
  | Tok_Mult -> "Tok_Mult"
  | Tok_LessEqual -> "Tok_LessEqual"
  | Tok_Less -> "Tok_Less"
  | Tok_LParen -> "Tok_LParen"
  | Tok_Int(i) -> "Tok_Int(" ^ (string_of_int i) ^ ")"
  | Tok_If -> "Tok_If"
  | Tok_ID(id) -> "Tok_ID(\"" ^ id ^ "\")"
  | Tok_String(s) -> "Tok_String(\"" ^ s ^ "\")"
  | Tok_GreaterEqual -> "Tok_GreaterEqual"
  | Tok_Greater -> "Tok_Greater"
  | Tok_Equal -> "Tok_Equal"
  | Tok_Then -> "Tok_Then"
  | Tok_Else -> "Tok_Else"
  | Tok_Div -> "Tok_Div"
  | Tok_Bool(b) -> "Tok_Bool(" ^ (string_of_bool b) ^ ")"
  | Tok_And -> "Tok_And"
  | Tok_Concat -> "Tok_Concat"
  | Tok_Let -> "Tok_Let"
  | Tok_Def -> "Tok_Def"
  | Tok_In -> "Tok_In"
  | Tok_Rec -> "Tok_Rec"
  | Tok_Arrow -> "Tok_Arrow"
  | Tok_Fun -> "Tok_Fun"
  | Tok_DoubleSemi -> "Tok_DoubleSemi"

let string_of_list ?newline:(newline=false) (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (String.concat ", " @@ List.map f l) ^ "]" ^ (if newline then "\n" else "");;

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let reRParen = Str.regexp "("
let reLParen = Str.regexp ")" 
let reEqu = Str.regexp "="
let reNotEqu = Str.regexp "<>"
let reGrtr = Str.regexp ">"
let reLss = Str.regexp "<"
let reGrEq = Str.regexp ">="
let reLsEq = Str.regexp "<="
let reOr = Str.regexp "||"
let reAnd = Str.regexp "&&"
let reNot = Str.regexp "not\\b"
let reIf = Str.regexp "if\\b"
let reThn = Str.regexp "then\\b"
let reEls = Str.regexp "else\\b"
let reAdd = Str.regexp "\\+"
let reSub = Str.regexp "-"
let reMult = Str.regexp "\\*"
let reDiv = Str.regexp "/"
let reCnct = Str.regexp "\\^"
let reLet = Str.regexp "let\\b"
let reDef = Str.regexp "def\\b"
let reIn = Str.regexp "in\\b"
let rerec = Str.regexp "rec\\b"
let reFun = Str.regexp "fun\\b"
let reArw = Str.regexp "->"
let reDubSmi = Str.regexp ";;"
let reBool = Str.regexp "true\\|false"
let reInt = Str.regexp "[0-9]+\\|(-[0-9]+)"
let reStr = Str.regexp {|\"[^\"]*\"|}
let reID = Str.regexp {|[a-zA-Z][a-zA-Z0-9]*|}
let reWht = Str.regexp "[ \n\r\x0c\t]" (*should match all white space characters*)
let tokenize input = 
    let rec scan pos s =
    if pos >= String.length s then
        []
    else
    (*all of these need to look for whitespace afterwards so they don't match a substring*)
        if Str.string_match reWht s pos then
            scan (pos + 1) s
        else if Str.string_match reNot s pos then
            Tok_Not::(scan (pos + 3) s)
        else if Str.string_match reIf s pos then
            Tok_If::(scan (pos + 2) s)
        else if Str.string_match reThn s pos then
            Tok_Then::(scan (pos + 4) s)
        else if Str.string_match reEls s pos then
            Tok_Else::(scan (pos + 4) s)
        else if Str.string_match reLet s pos then
            Tok_Let::(scan (pos + 3) s)
        else if Str.string_match reDef s pos then
            Tok_Def::(scan (pos + 3) s)
        else if Str.string_match reIn s pos then
            Tok_In::(scan (pos + 2) s)
        else if Str.string_match rerec s pos then
            Tok_Rec::(scan (pos + 3) s)
        else if Str.string_match reFun s pos then
            Tok_Fun::(scan (pos + 3) s)
        else if Str.string_match reBool s pos then
            let found = Str.matched_string s in
            if (Str.string_match (Str.regexp "true") found 0) then
                (Tok_Bool true)::(scan (pos + 4) s)
            else (Tok_Bool false)::(scan (pos + 5) s)
        else if Str.string_match reID s pos then 
            let found = Str.matched_string s in
            let len = String.length found in
            (Tok_ID found)::(scan (pos + len) s)
        else if Str.string_match reInt s pos then
            let found = Str.matched_string s in
            let len = String.length found in
            if found.[0] = '(' then
                let clean = String.sub found 1 (len - 2) in
                (Tok_Int (int_of_string clean))::(scan (pos + len) s)
            else (Tok_Int (int_of_string found))::(scan (pos + len) s)
        else if Str.string_match reRParen s pos then
            (Tok_RParen)::(scan (pos + 1) s)
        else if Str.string_match reLParen s pos then
            Tok_LParen::(scan (pos + 1) s)
        else if Str.string_match reArw s pos then
            Tok_Arrow::(scan (pos + 2) s)
        else if Str.string_match reEqu s pos then
            Tok_Equal::(scan (pos + 1) s)
        else if Str.string_match reNotEqu s pos then
            Tok_NotEqual::(scan (pos + 2) s)
        else if Str.string_match reGrEq s pos then
            Tok_GreaterEqual::(scan (pos + 2) s)
        else if Str.string_match reGrtr s pos then 
            Tok_Greater::(scan (pos + 1) s)
        else if Str.string_match reLsEq s pos then
            Tok_LessEqual::(scan (pos + 2) s)
        else if Str.string_match reLss s pos then
            Tok_Less::(scan (pos + 1) s)
        else if Str.string_match reOr s pos then
            Tok_Or::(scan (pos + 2) s)
        else if Str.string_match reAnd s pos then
            Tok_And::(scan (pos + 2) s)
        else if Str.string_match reAdd s pos then
            Tok_Add::(scan (pos + 1) s)
        else if Str.string_match reSub s pos then
            Tok_Sub::(scan (pos + 1) s)
        else if Str.string_match reMult s pos then
            Tok_Mult::(scan (pos + 1) s)
        else if Str.string_match reDiv s pos then
            Tok_Div::(scan (pos + 1) s)
        else if Str.string_match reCnct s pos then
            Tok_Concat::(scan (pos + 1) s)
        else if Str.string_match reDubSmi s pos then
            Tok_DoubleSemi::(scan (pos + 2) s)
        else if Str.string_match reStr s pos then
            let found = Str.matched_string s in
            let len = String.length found in
            let clean = String.sub s (pos + 1) (len - 2) in
            (Tok_String clean)::(scan (pos + len) s)
        else if pos >= (String.length s) then
            []
        else
            raise (InvalidInputException s)
    in
    let out = scan 0 input in
    (*let _ = print_endline input in
    let _ = print_endline (string_of_list (fun a -> string_of_token a) out) in*)
    out;;


