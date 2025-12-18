open Token

(*
This file uses proof-archivist for fast, batched symbol resolution

https://gitlab.com/cogumbreiro/proof-archivist/
*)

let parse_kind (s : string) : semantic_kind option =
  match s with
  | "Inductive" -> Some Type
  | "Constructor" -> Some (Constructor [])
  | "Definition" | "Constant" | "Fixpoint" | "Lemma" | "Theorem" -> Some Function
  | "Notation" -> Some Function  (* Treat notations as functions for highlighting *)
  | "Module" -> Some Module
  | "Ltac" | "Ltac2" -> Some Ltac
  | _ -> None

(* Extract short name from fully qualified name like "Corelib.Init.Datatypes.nat" -> "nat" *)
let short_name (qualified : string) : string =
  match String.rindex_opt qualified '.' with
  | Some i -> String.sub qualified (i + 1) (String.length qualified - i - 1)
  | None -> qualified

(* Parse a single line of proof-query output: "Constructor Corelib.Init.Datatypes.S" *)
let parse_line (line : string) : (string * semantic_kind) option =
  let line = String.trim line in
  if line = "" then None
  else
    match String.index_opt line ' ' with
    | None -> None
    | Some i ->
        let kind_str = String.sub line 0 i in
        let qualified = String.sub line (i + 1) (String.length line - i - 1) |> String.trim in
        match parse_kind kind_str with
        | None -> None
        | Some kind -> Some (short_name qualified, kind)

(* Run proof-query locate with a list of symbols, return a hashtable of results *)
let locate_batch (symbols : string list) : (string, semantic_kind) Hashtbl.t =
  let table = Hashtbl.create (List.length symbols) in
  if symbols = [] then table
  else
    let args = List.concat_map (fun s -> ["--symbol"; s]) symbols in
    let cmd = "proof-query" in
    let argv = Array.of_list (cmd :: "locate" :: args) in
    
    (* Create process and read output *)
    let (stdout_read, stdout_write) = Unix.pipe () in
    let pid = 
      try
        Unix.create_process cmd argv Unix.stdin stdout_write Unix.stderr
      with Unix.Unix_error (err, _, _) ->
        Unix.close stdout_read;
        Unix.close stdout_write;
        Printf.eprintf "Warning: Could not run proof-query: %s\n" (Unix.error_message err);
        -1
    in
    
    if pid < 0 then table
    else begin
      Unix.close stdout_write;
      let ic = Unix.in_channel_of_descr stdout_read in
      
      (* Read all lines *)
      let rec read_lines () =
        match input_line ic with
        | line ->
            (match parse_line line with
             | Some (name, kind) -> Hashtbl.replace table name kind
             | None -> ());
            read_lines ()
        | exception End_of_file -> ()
      in
      read_lines ();
      close_in ic;
      
      (* Wait for process *)
      (try ignore (Unix.waitpid [] pid) with _ -> ());
      table
    end

(* Legacy interface for compatibility - wraps batched lookup *)
type t = unit

let start () : t = ()

let locate (_t : t) (ident : string) : semantic_kind option =
  let results = locate_batch [ident] in
  Hashtbl.find_opt results ident

let close (_t : t) = ()
