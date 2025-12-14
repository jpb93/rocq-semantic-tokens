open Token

let collect_globals (tokens : token list) =
  let table = Hashtbl.create 64 in
  
  let add name kind = Hashtbl.add table name kind in
  
  let rec loop = function
    | [] -> table
    
    | { kind = KEYWORD k; _ } :: { kind = IDENT name; _ } :: rest
      when List.mem k ["Definition"; "Fixpoint"; "Theorem"; "Lemma"; 
                       "Remark"; "Fact"; "Corollary"; "Proposition"; 
                       "Example"; "Axiom"; "Conjecture"; "Parameter";
                       "Variable"; "Hypothesis"; "Instance"] ->
        add name Function;
        loop rest
    
    | { kind = KEYWORD k; _ } :: { kind = IDENT name; _ } :: rest
      when List.mem k ["Inductive"; "CoInductive"; "Record"; "Structure"; "Class"] ->
        add name Type;
        loop (scan_constructors rest)
    
    | { kind = KEYWORD "Module"; _ } :: { kind = IDENT name; _ } :: rest ->
        add name Module;
        loop rest
    
    | _ :: rest -> loop rest
  
  and scan_constructors = function
    | [] -> []
    | { kind = DOT; _ } :: rest -> rest
    
    | { kind = PIPE; _ } :: { kind = IDENT name; _ } :: rest ->
        add name Constructor;
        scan_constructors rest
    
    | { kind = COLONEQUAL; _ } :: { kind = IDENT name; _ } :: rest ->
        add name Constructor;
        scan_constructors rest
        
    | _ :: rest -> scan_constructors rest
  in
  
  loop tokens
