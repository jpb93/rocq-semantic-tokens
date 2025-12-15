type t =
  | Ident of string  (* atomic identifier such as nat, bool, Prop, etc.*)
  | Arrow of t * t   (* A -> B *)
  | App of t * t     (* list nat, option a *)
  
let rec type_expr_to_string = function
  | Ident s -> s
  | Arrow (l, r) -> 
      "(" ^ type_expr_to_string l ^ " -> " ^ type_expr_to_string r ^ ")"
  | App (l, r) -> 
      "(" ^ type_expr_to_string l ^ " " ^ type_expr_to_string r ^ ")"

let is_function_type = function
  | Arrow _ -> true
  | _ -> false

(*
  Extracts parameter types from a function type. For example:
   nat -> nat -> wrapper  =>  ([nat; nat], wrapper)
   nat                    =>  ([], nat)
*)
let rec extract_params = function
  | Arrow (left, right) ->
      let (rest_params, return_type) = extract_params right in
      (left :: rest_params, return_type)
  | t -> ([], t)
