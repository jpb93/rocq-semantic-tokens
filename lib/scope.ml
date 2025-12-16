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
  
type output = semantic_token list

let analyze ~(globals: (string, semantic_kind) Hashtbl.t) (tokens : token list) : output =
  
  let results = ref [] in
  let emit tok = results := tok :: !results in
  
  let emit_token (t : token) (kind : semantic_kind) =
    emit { pos = t.byte_offset; len = t.len; sem_kind = kind}
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
    
    (* Parameter binding: (name : type) *)
    | { Token.kind = LPAREN; _ } :: ({ Token.kind = IDENT name; _ } as t) :: { Token.kind = COLON; _ } :: rest ->
        emit_token t Variable;
        loop (add_binding name Variable stack) rest
    
    (* End of definition *)
    | { Token.kind = DOT; _ } :: rest ->
        if stack = empty_stack then
          loop stack rest
        else
          loop (close_scope stack) rest
          
    (* Match start *)
    | { Token.kind = KEYWORD "match"; _} :: rest ->
      loop (open_scope stack) rest
      
    | { Token.kind = PIPE; _} :: ({ Token.kind = IDENT cname; _} as t) :: rest ->
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
            loop stack rest)
            
    (* Wildcard *)
    | { Token.kind = PIPE; _ } :: { Token.kind = UNDERSCORE; _ } :: rest ->
      let stack = close_scope stack in
      let stack = open_scope stack in
      loop stack rest      
      
    (* Match end *)
    | { Token.kind = KEYWORD "end"; _ } :: rest ->
      loop (close_scope stack) rest
      
    (* let binding: let x := ... in ... *)
    | { Token.kind = KEYWORD "let"; _ } 
      :: ({ Token.kind = IDENT name; _ } as t) 
      :: { Token.kind = COLONEQUAL; _ } :: rest ->
        emit_token t Variable;
        loop (add_binding name Variable stack) rest
    
    (* Any identifier: look up in scope, then globals *)
    | ({ Token.kind = IDENT name; _ } as t) :: rest ->
        (match find_local name stack with
         | Some kind -> emit_token t kind
         | None ->
             match Hashtbl.find_opt globals name with
             | Some kind -> emit_token t kind
             | None -> ());
        loop stack rest
    | _ :: rest -> loop stack rest
  in
  
  loop empty_stack tokens;
  List.rev !results

