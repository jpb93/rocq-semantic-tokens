open Coq_tokens

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <file.v>\n" Sys.argv.(0);
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let n = in_channel_length ic in
  let source = really_input_string ic n in
  close_in ic;
  let tokens = Lexer.lex source in
  let globals = Globals.collect_globals tokens in
  Hashtbl.iter (fun name kind ->
    Printf.printf "%s -> %s\n" name (Token.semantic_kind_to_string kind)
  ) globals

