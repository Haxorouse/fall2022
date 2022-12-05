open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
    | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
    | IntNode (low, None, _, _, _) -> 
      print_int low;
      print_int x;
      if x < low then IntNode (x, Some low, IntLeaf, IntLeaf, IntLeaf)
      else if x > low then IntNode (low, Some x, IntLeaf, IntLeaf, IntLeaf)
      else t
    | IntNode (low, Some high, fst, scd, thd) ->
      if x < low then IntNode(low, Some high, int_insert x fst, scd, thd)
      else if x > high then IntNode(low, Some high, fst, scd, int_insert x thd)
      else if x > low && x < high then IntNode(low, Some high, fst, int_insert x scd, thd)
      else t;;

let rec int_mem x t =
  match t with
    | IntLeaf -> false
    | IntNode (low, None, _, _, _) -> if x = low then true else false
    | IntNode (low, Some high, fst, scd, thd) -> 
      if x = low || x = high then true
      else if x < low then int_mem x fst
      else if x > high then int_mem x thd
      else int_mem x scd;;

let rec int_size t =
   match t with
    | IntLeaf -> 0
    | IntNode (low, None, _, _, _) -> 1
    | IntNode (low, Some high, fst, scd, thd) ->
      (2 + int_size fst + int_size scd + int_size thd);;

let rec int_max t =
  match t with
    | IntLeaf -> raise (Invalid_argument("int_max"))
    | IntNode (low, None, _, _, _) -> low
    | IntNode (low, Some high, _, _, IntLeaf) -> high
    | IntNode (low, Some high, _, _, thd) -> int_max thd;;

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let getKey pair =
  match pair with
  (x, _) -> x;;

let rec map_put k v t = 
  let mapped = (k, v) in
  match t with
    | MapLeaf -> MapNode(mapped, None, MapLeaf, MapLeaf, MapLeaf)
    | MapNode (low, None, _, _, _) -> 
      let key = getKey low in
      if k < key then MapNode (mapped, Some low, MapLeaf, MapLeaf, MapLeaf)
      else if k > key then MapNode (low, Some mapped, MapLeaf, MapLeaf, MapLeaf)
      else
        raise (Invalid_argument("map_put"))
    | MapNode (low, Some high, fst, scd, thd) ->
      let keyL = getKey low in
      let keyH = getKey high in
      if k < keyL then MapNode(low, Some high, map_put k v fst, scd, thd)
      else if k > keyH then MapNode(low, Some high, fst, scd, map_put k v thd)
      else if k > keyL && k < keyH then MapNode(low, Some high, fst, map_put k v scd, thd)
      else
        raise (Invalid_argument("map_put"));;

let rec map_contains k t = 
  match t with
    | MapLeaf -> false
    | MapNode (low, None, _, _, _) -> if k = (getKey low) then true else false
    | MapNode (low, Some high, fst, scd, thd) ->
      let keyL = getKey low in
      let keyH = getKey high in
      if k = keyL || k = keyH then true
      else if k < keyL then map_contains k fst
      else if k > keyH then map_contains k thd
      else map_contains k scd

let rec map_get k t =
  let getValue (k, v) = v in
  match t with
    | MapLeaf -> raise (Invalid_argument("map_get"))
    | MapNode (low, None, _, _, _) -> if k = (getKey low) 
      then getValue low else raise (Invalid_argument("map_get"))
    | MapNode (low, Some high, fst, scd, thd) ->
      let keyL = getKey low in
      let keyH = getKey high in
      if k = keyL then getValue low
      else if k = keyH then getValue high
      else if k < keyL then map_get k fst
      else if k > keyH then map_get k thd
      else map_get k scd

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = unit

let empty_table : lookup_table = ()

let push_scope (table : lookup_table) : lookup_table = 
  failwith "unimplemented"

let pop_scope (table : lookup_table) : lookup_table =
  failwith "unimplemented"

let add_var name value (table : lookup_table) : lookup_table =
  failwith "unimplemented"

let rec lookup name (table : lookup_table) =
  failwith "unimplemented"
