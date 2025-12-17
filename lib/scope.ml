open Token

type binding = {
  name : string;
  kind : semantic_kind;
}

type scope = binding list

type scope_stack = scope list

let empty_stack : scope_stack = []

let open_scope (stack : scope_stack) : scope_stack = [] :: stack

let close_scope (stack : scope_stack) : scope_stack =
  match stack with
  | [] -> failwith "close_scope: empty stack"
  | _ :: rest -> rest
  
let add_binding (name : string) (kind : semantic_kind) (stack : scope_stack) : scope_stack =
  match stack with
  | [] -> [[{ name; kind}]]
  | scope :: rest -> ({ name; kind } :: scope) :: rest
  
let find_local (name : string) (stack : scope_stack) : semantic_kind option =
  let rec search = function
  | [] -> None
  | scope :: rest ->
      match List.find_opt (fun b -> b.name = name) scope with
      | Some b -> Some b.kind
      | None -> search rest
  in
  search stack

type output = {
  tokens : semantic_token list;
  unknowns : string list;
}

let analyze ~(globals: (string, semantic_kind) Hashtbl.t) (tokens : token list) : output =
  
  let results = ref [] in
  let unknowns_ref = ref [] in
  let emit tok = results := tok :: !results in
  
  let emit_token (t : token) (kind : semantic_kind) =
    emit { pos = t.byte_offset; len = t.len; sem_kind = kind}
  in
  
  (* Emit tokens for identifiers in a type expression.
     If an identifier is bound in scope, emit with its bound kind.
     Otherwise emit as Type (it's a type name like nat, list, etc.) *)
  let emit_type_expr_tokens stack start stop =
    let stop_pos = match stop with
      | t :: _ -> t.Token.byte_offset
      | [] -> max_int
    in
    let rec emit = function
      | [] -> ()
      | t :: _ when t.Token.byte_offset >= stop_pos -> ()
      | { Token.kind = IDENT name; _ } as t :: rest ->
          (* Check if this identifier is bound locally (e.g., type parameter) *)
          (match find_local name stack with
           | Some kind -> emit_token t kind  (* Use bound kind - likely Variable for type params *)
           | None -> emit_token t Type);     (* Not bound - it's a type name *)
          emit rest
      | { Token.kind = KEYWORD s; _ } as t :: rest 
        when List.mem s ["Type"; "Prop"; "Set"] ->
          emit_token t Type;
          emit rest
      | _ :: rest -> emit rest
    in
    emit start
  in
  
  let bind_multi_params stack tokens =
    let rec collect_idents acc = function
      | { Token.kind = COLON; _ } :: rest -> (List.rev acc, rest)
      | ({ Token.kind = IDENT _; _ } as t) :: rest -> collect_idents (t :: acc) rest
      | rest -> (List.rev acc, rest)
    in
    let (ident_tokens, after_colon) = collect_idents [] tokens in
    let (type_expr, rest') = Type_parser.parse_type_expr after_colon in
    
    let kind = if Type_expr.is_function_type type_expr then Function else Variable in
    (* Emit variable tokens first (they appear first in source) *)
    let stack' = List.fold_left (fun stk t ->
      let name = match t.Token.kind with Token.IDENT n -> n | _ -> "" in
      emit_token t kind;
      add_binding name kind stk
    ) stack ident_tokens in
    (* Then emit type tokens for identifiers in the type annotation *)
    (* Pass stack' so type parameters (A, B, etc.) are recognized as bound variables *)
    emit_type_expr_tokens stack' after_colon rest';
    (stack', rest')
  in  
  
  let rec bind_pattern_vars param_types stack tokens =
    match tokens, param_types with
    | [], _ -> (stack, [])
    | { Token.kind = FATARROW; _ } :: _, _ -> (stack, tokens)
    
    | { Token.kind = UNDERSCORE; _ } :: rest, _ :: ptypes ->
        bind_pattern_vars ptypes stack rest
        
    | { Token.kind = UNDERSCORE; _ } :: rest, [] ->
        bind_pattern_vars [] stack rest
    
    | ({ Token.kind = IDENT name; _ } as t) :: rest, ptype :: ptypes ->
        let kind = if Type_expr.is_function_type ptype then Function else Variable in
        emit_token t kind;
        bind_pattern_vars ptypes (add_binding name kind stack) rest
    
    | ({ Token.kind = IDENT name; _ } as t) :: rest, [] ->
        emit_token t Variable;
        bind_pattern_vars [] (add_binding name Variable stack) rest
    
    | _, _ -> (stack, tokens)
  in
  
  let rec loop stack tokens =
    match tokens with
    | [] -> ()
    
    (* Definition/Fixpoint: emit name as Function, push scope *)
    | { Token.kind = KEYWORD k; _ } :: ({ Token.kind = IDENT _name; _ } as t) :: rest
      when List.mem k Keywords.function_definining_keywords ->
        emit_token t Function;
        loop (open_scope stack) rest
    
    (* Inductive/Record: emit name as Type, push scope for constructors *)
    | { Token.kind = KEYWORD k; _ } :: ({ Token.kind = IDENT _name; _ } as t) :: rest
      when List.mem k Keywords.type_defining_keywords ->
        emit_token t Type;
        loop (open_scope stack) rest
    
    (* Module *)
    | { Token.kind = KEYWORD "Module"; _ } :: ({ Token.kind = IDENT _name; _ } as t) :: rest ->
      emit_token t Module;
      loop stack rest
    
    (* Parameter binding: (name ... : type) - handles (x : T) and (x y : T) *)
    (* Parameter binding: (name ... : type) *)
    | { Token.kind = LPAREN; _ } 
      :: ({ Token.kind = IDENT _; _ } 
      :: rest_after_first_ident as rest) ->
        (* Check if this looks like a parameter binding by scanning for colon *)
        let rec has_colon_before_paren = function
          | [] -> false
          | { Token.kind = COLON; _ } :: _ -> true
          | { Token.kind = RPAREN; _ } :: _ -> false
          | { Token.kind = IDENT _; _ } :: rest -> has_colon_before_paren rest
          | _ :: _ -> false
        in
        if has_colon_before_paren rest_after_first_ident then
          let (stack', rest') = bind_multi_params stack rest in
          loop stack' rest'
        else
          loop stack rest  (* Just skip non-parameter parens *)
    
    (* End of definition *)
    | { Token.kind = DOT; _ } :: rest ->
        if stack = empty_stack then
          loop stack rest
        else
          loop (close_scope stack) rest
          
    (* Implicit parameter binding: {name : type} *)
    | { Token.kind = LBRACE; _ } :: ({ Token.kind = IDENT _; _ } :: rest_after_ident as rest) ->
      let rec has_colon_immediately = function
        | { Token.kind = COLON; _ } :: _ -> true
        | { Token.kind = IDENT _; _ } :: rest -> has_colon_immediately rest
        | _ -> false
      in
      if has_colon_immediately rest_after_ident then
        let (stack', rest') = bind_multi_params stack rest in
        loop stack' rest'
      else
        loop stack rest
        
    (* Match start *)
    | { Token.kind = KEYWORD "match"; _} :: rest ->
      loop (open_scope stack) rest
      
    | { Token.kind = PIPE; _} :: ({ Token.kind = IDENT cname; _} as t) :: rest ->
      if stack = empty_stack then
        (* Not in a match context, skip this pipe (e.g., intro patterns) *)
        loop stack rest
      else begin
        let stack = close_scope stack in
        let stack = open_scope stack in
        (match Hashtbl.find_opt globals cname with
          | Some (Constructor params) ->
              emit_token t (Constructor params);
              let (stack', rest') = bind_pattern_vars params stack rest in
              loop stack' rest'
          | Some kind -> 
              emit_token t kind;
              loop stack rest
          | None ->
              (* If a constructor only appears in patterns, we still want to query coqtop for it. *)
              if not (List.mem cname !unknowns_ref) then
                unknowns_ref := cname :: !unknowns_ref;
              loop stack rest)
        end
            
    (* Wildcard *)
    | { Token.kind = PIPE; _ } :: { Token.kind = UNDERSCORE; _ } :: rest ->
      if stack = empty_stack then
        loop stack rest
      else begin
        let stack = close_scope stack in
        let stack = open_scope stack in
        loop stack rest      
      end
      
    (* Match end *)
    | { Token.kind = KEYWORD "end"; _ } :: rest ->
      loop (close_scope stack) rest
      
    (* let binding to a function: let f := fun ... *)
    | { Token.kind = KEYWORD "let"; _ } 
      :: ({ Token.kind = IDENT name; _ } as t) 
      :: { Token.kind = COLONEQUAL; _ } 
      :: ({ Token.kind = KEYWORD "fun"; _ } :: _ as rest) ->
        emit_token t Function;
        loop (add_binding name Function stack) rest
        
    (* let binding: let x := ... in ... *)
    | { Token.kind = KEYWORD "let"; _ } 
      :: ({ Token.kind = IDENT name; _ } as t) 
      :: { Token.kind = COLONEQUAL; _ } 
      :: rest ->
        emit_token t Variable;
        loop (add_binding name Variable stack) rest
        
    | { Token.kind = KEYWORD "fun"; _ } 
      :: ({ Token.kind = IDENT name; _ } as t) 
      :: { Token.kind = FATARROW; _ } 
      :: rest ->
        emit_token t Variable;
        loop (add_binding name Variable stack) rest
        
    (* forall binding: forall x, ... *)
    | { Token.kind = KEYWORD "forall"; _ } 
      :: ({ Token.kind = IDENT name; _ } as t) 
      :: { Token.kind = COMMA; _ } :: rest ->
        
        emit_token t Variable;
        loop (add_binding name Variable stack) rest
        
    (* Any identifier: look up in scope, then globals, or mark unknown *)
    | ({ Token.kind = IDENT name; _ } as t) :: rest ->
        (match find_local name stack with
         | Some kind -> emit_token t kind
         | None ->
           match Hashtbl.find_opt globals name with
             | Some kind -> emit_token t kind
             | None -> 
               if not (List.mem name !unknowns_ref) then
                 unknowns_ref := name :: !unknowns_ref);
        loop stack rest
    | _ :: rest -> loop stack rest
    
  in
  
  loop empty_stack tokens;
  { tokens = List.rev !results; unknowns = List.rev !unknowns_ref }
