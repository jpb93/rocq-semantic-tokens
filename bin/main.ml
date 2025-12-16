open Coq_tokens

let kind_to_json_string = function
  | Token.Type -> "type"
  | Token.Constructor _ -> "constructor"
  | Token.Function -> "function"
  | Token.Module -> "module"
  | Token.Variable -> "variable"

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
      text
  ) tokens in
  print_endline ("[" ^ String.concat "," json_tokens ^ "]")

let () =
  let json_mode = ref false in
  let filename = ref "" in
  
  let args = Array.to_list Sys.argv |> List.tl in
  List.iter (fun arg ->
    if arg = "--json" then json_mode := true
    else filename := arg
  ) args;
  
  if !filename = "" then begin
    Printf.eprintf "Usage: %s [--json] <file.v>\n" Sys.argv.(0);
    exit 1
  end;
  
  let ic = open_in !filename in
  let n = in_channel_length ic in
  let source = really_input_string ic n in
  close_in ic;
  
  let tokens = Lexer.lex source in
  let globals = Globals.collect_globals tokens in
  let sem_tokens = Scope.analyze ~globals tokens in
  
  if !json_mode then
    print_json source sem_tokens
  else
    print_human source sem_tokens
