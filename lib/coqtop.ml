open Token

type t = {
  pid   : int;
  stdin : out_channel;
  out   : in_channel;  (* merged stdout+stderr *)
}

let set_cloexec fd = Unix.set_close_on_exec fd

let update_tail (tail : string) (c : char) : string =
  let tail' = tail ^ String.make 1 c in
  let max_len = 64 in
  let len = String.length tail' in
  if len <= max_len then tail' else String.sub tail' (len - max_len) max_len

(* Detect a trailing prompt like "Coq < " or "Foo_bar' < " at end of stream.
   Returns Some suffix_len_to_strip if present. *)
let prompt_suffix_len (tail : string) : int option =
  if not (String.ends_with ~suffix:" < " tail) then None
  else
    let len = String.length tail in
    let end_word = len - 4 in (* last char of the prompt "word" *)
    let rec find_line_start i =
      if i < 0 then 0
      else
        match tail.[i] with
        | '\n' | '\r' -> i + 1
        | _ -> find_line_start (i - 1)
    in
    let start0 = find_line_start end_word in
    let word = String.sub tail start0 (end_word - start0 + 1) |> String.trim in
    let is_ok_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' -> true
      | _ -> false
    in
    if word = "" || not (String.for_all is_ok_char word) then None
    else Some (String.length word + 3) (* "word" + " < " *)

let read_until_prompt (t : t) : string =
  let buffer = Buffer.create 1024 in
  let tail = ref "" in
  let rec loop () =
    let c = input_char t.out in
    Buffer.add_char buffer c;
    tail := update_tail !tail c;
    match prompt_suffix_len !tail with
    | Some n ->
        let s = Buffer.contents buffer in
        let total = String.length s in
        let body = if total >= n then String.sub s 0 (total - n) else s in
        String.trim body
    | None -> loop ()
  in
  try loop () with End_of_file -> Buffer.contents buffer |> String.trim
  
(* If a stale prompt was already sitting in the pipe, the first read can come back empty.
   For Locate queries we never want to treat that as the real response. *)
let rec read_nonempty_until_prompt (t : t) : string =
  let s = read_until_prompt t in
  if String.trim s = "" then read_nonempty_until_prompt t else s

let send_command (t : t) (cmd : string) =
  output_string t.stdin (cmd ^ "\n");
  flush t.stdin

let strip_prompt_prefix (line : string) : string =
  let s = String.trim line in
  match String.index_opt s '<' with
  | Some i when i > 0 && s.[i - 1] = ' ' ->
      (* "XYZ < rest" -> "rest" *)
      let rest =
        if i + 1 < String.length s then String.sub s (i + 1) (String.length s - i - 1)
        else ""
      in
      String.trim rest
  | _ -> s

let parse_locate_response (response : string) : Token.semantic_kind option =
  let result = response
  |> String.split_on_char '\n'
  |> List.map strip_prompt_prefix
  |> List.filter (fun s -> s <> "" && not (String.starts_with ~prefix:"Welcome to" s))
  |> List.find_opt (fun s ->
       String.starts_with ~prefix:"No object" s
       || String.starts_with ~prefix:"Constant" s
       || String.starts_with ~prefix:"Inductive" s
       || String.starts_with ~prefix:"Constructor" s
       || String.starts_with ~prefix:"Module" s
       || String.starts_with ~prefix:"Ltac" s)
  |> function
     | None -> None
     | Some s when String.starts_with ~prefix:"No object" s -> None
     | Some s when String.starts_with ~prefix:"Constant" s -> Some Function
     | Some s when String.starts_with ~prefix:"Inductive" s -> Some Type
     | Some s when String.starts_with ~prefix:"Constructor" s -> Some (Constructor [])
     | Some s when String.starts_with ~prefix:"Module" s -> Some Module
     | Some s when String.starts_with ~prefix:"Ltac" s -> Some Ltac

     | _ -> None
  in
  result

let start () : t =
  let (stdin_read, stdin_write) = Unix.pipe () in
  let (out_read, out_write) = Unix.pipe () in

  (* Prevent the child from inheriting the parent-only ends. *)
  set_cloexec stdin_write;
  set_cloexec out_read;

  let pid =
    Unix.create_process "coqtop"
      [| "coqtop"; "-quiet" |]
      stdin_read
      out_write
      out_write (* merge stderr into stdout *)
  in

  Unix.close stdin_read;
  Unix.close out_write;

  let t =
    { pid
    ; stdin = Unix.out_channel_of_descr stdin_write
    ; out   = Unix.in_channel_of_descr out_read
    }
  in

  (* Warm-up without desync: if a stale prompt is already available, the first read can be empty;
     consume until we actually get the Locate output. *)
  ignore (read_nonempty_until_prompt t);

  t

let locate (t : t) (ident : string) : Token.semantic_kind option =
  send_command t (Printf.sprintf "Locate %s." ident);
  read_nonempty_until_prompt t |> parse_locate_response

let close (t : t) =
  (* Try to exit cleanly. *)
  (try send_command t "Quit." with _ -> ());
  (try close_out_noerr t.stdin with _ -> ());
  (try close_in_noerr t.out with _ -> ());
  ignore (Unix.waitpid [] t.pid)
