open Coq_tokens

let kind_to_json_string = function
  | Token.Type -> "type"
  | Token.Constructor _ -> "constructor"
  | Token.Function -> "function"
  | Token.Module -> "module"
  | Token.Variable -> "variable"
  | Token.Ltac -> "ltac"
  
let json_escape (s : string) : string =
  let b = Buffer.create (String.length s + 16) in
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\b' -> Buffer.add_string b "\\b"
    | '\012' -> Buffer.add_string b "\\f"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c when Char.code c < 0x20 ->
        Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let print_human source tokens =
  List.iter (fun (tok : Token.semantic_token) ->
    let text = String.sub source tok.pos tok.len in
    Printf.printf "%d:%d %s \"%s\"\n" 
      tok.pos 
      tok.len 
      (Token.semantic_kind_to_string tok.sem_kind) 
      text
  ) tokens

let print_json source tokens =
  let json_tokens = List.map (fun (tok : Token.semantic_token) ->
    let text = String.sub source tok.pos tok.len in
    Printf.sprintf {|{"pos":%d,"len":%d,"kind":"%s","text":"%s"}|}
      tok.pos
      tok.len
      (kind_to_json_string tok.sem_kind)
      (json_escape text)
  ) tokens in
  print_endline ("[" ^ String.concat "," json_tokens ^ "]")

let () =
  let json_mode = ref false in
  let query_coqtop_flag = ref false in
  let filename = ref "" in
  
  let args = Array.to_list Sys.argv |> List.tl in
  List.iter (fun arg ->
    if arg = "--json" then json_mode := true
    else if arg = "--query-coqtop" then query_coqtop_flag := true
    else filename := arg
  ) args;
  
  if !filename = "" then begin
    Printf.eprintf "Usage: %s [--json] [--query-coqtop] <file.v>\n" Sys.argv.(0);
    exit 1
  end;
  
  let ic = open_in !filename in
  let n = in_channel_length ic in
  let source = really_input_string ic n in
  close_in ic;
  
  let tokens = Lexer.lex source in
  let globals = Globals.collect_globals tokens in
  let result = Scope.analyze ~globals tokens in
  
  (* If --query-coqtop, resolve unknowns *)
  let all_tokens =
    if !query_coqtop_flag && List.length result.unknowns > 0 then begin
      (* Filter to only query things likely to be global *)
      let queryable_unknowns = List.filter (fun name ->
        String.length name > 1 || 
        (String.length name = 1 && name.[0] >= 'A' && name.[0] <= 'Z')
      ) result.unknowns in
      
      if List.length queryable_unknowns = 0 then
        result.tokens
      else begin
        let coq = Coqtop.start () in
        let resolved = List.filter_map (fun ident ->
          match Coqtop.locate coq ident with
          | Some kind -> Some (ident, kind)
          | None -> None
        ) queryable_unknowns in  (* Changed from result.unknowns *)
        Coqtop.close coq;
        
        
        (* IMPORTANT: do NOT substring-scan the raw source (breaks on 1-letter idents like S/O).
           Instead, enrich globals and re-run analysis so positions come from the lexer. *)
        List.iter (fun (name, kind) ->
          if not (Hashtbl.mem globals name) then Hashtbl.add globals name kind
        ) resolved;

        let result2 = Scope.analyze ~globals tokens in
        result2.tokens
        
      end
    end else
      result.tokens
  in
  
  if !json_mode then
    print_json source all_tokens
  else
    print_human source all_tokens
