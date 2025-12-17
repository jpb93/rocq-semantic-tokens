open Token

type t = {
  pid   : int;
  stdin : out_channel;
  out   : in_channel;  (* merged stdout+stderr *)
}

exception Rocq_read_timeout of { tail : string; seconds : float }

let set_cloexec fd = Unix.set_close_on_exec fd

let update_tail (tail : string) (c : char) : string =
  let tail' = tail ^ String.make 1 c in
  let max_len = 256 in
  let len = String.length tail' in
  if len <= max_len then tail' else String.sub tail' (len - max_len) max_len

let rtrim_index (s : string) : int =
  let rec loop i =
    if i < 0 then -1
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> loop (i - 1)
      | _ -> i
  in
  loop (String.length s - 1)

(* Detect trailing prompt: "<WORD> <" optionally followed by whitespace.
   Returns suffix_len_to_strip from the *end* of the stream. *)
let prompt_suffix_len (tail : string) : int option =
  let len = String.length tail in
  let last = rtrim_index tail in
  if last < 2 then None
  else if tail.[last] <> '<' || tail.[last - 1] <> ' ' then None
  else
    let ws_after_lt = (len - 1) - last in
    let word_end = last - 2 in
    let rec find_line_start i =
      if i < 0 then 0
      else
        match tail.[i] with
        | '\n' | '\r' -> i + 1
        | _ -> find_line_start (i - 1)
    in
    let start0 = find_line_start word_end in
    let word =
      String.sub tail start0 (word_end - start0 + 1)
      |> String.trim
    in
    let is_ok_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' | '-' -> true
      | _ -> false
    in
    if word = "" || not (String.for_all is_ok_char word) then None
    else
      (* word + " <" + (optional whitespace after '<') *)
      Some (String.length word + 2 + ws_after_lt)

let read_until_prompt ?(timeout_s = 5.0) (t : t) : string =
  let buffer = Buffer.create 1024 in
  let tail = ref "" in
  let fd = Unix.descr_of_in_channel t.out in
  let deadline = Unix.gettimeofday () +. timeout_s in

  let rec loop () =
    let now = Unix.gettimeofday () in
    if now >= deadline then
      raise (Rocq_read_timeout { tail = !tail; seconds = timeout_s });

    let remaining = deadline -. now in
    let (readable, _, _) = Unix.select [fd] [] [] remaining in
    match readable with
    | [] -> loop ()
    | _ ->
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

let send_command (t : t) (cmd : string) =
  output_string t.stdin (cmd ^ "\n");
  flush t.stdin

let strip_prompt_prefix (line : string) : string =
  let s = String.trim line in
  match String.index_opt s '<' with
  | Some i when i > 0 && s.[i - 1] = ' ' ->
      let rest =
        if i + 1 < String.length s then String.sub s (i + 1) (String.length s - i - 1)
        else ""
      in
      String.trim rest
  | _ -> s

let parse_locate_response (response : string) : Token.semantic_kind option =
  response
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

let start () : t =
  let (stdin_read, stdin_write) = Unix.pipe () in
  let (out_read, out_write) = Unix.pipe () in

  set_cloexec stdin_write;
  set_cloexec out_read;

  let pid =
    Unix.create_process "rocq"
      [| "rocq"; "repl"; "-q"; "-color"; "off" |]
      stdin_read
      out_write
      out_write
  in

  Unix.close stdin_read;
  Unix.close out_write;

  let t =
    { pid
    ; stdin = Unix.out_channel_of_descr stdin_write
    ; out   = Unix.in_channel_of_descr out_read
    }
  in

  (* Force “first prompt” to happen after *a command*, not at startup. *)
  send_command t "Check nat.";
  ignore (read_until_prompt ~timeout_s:10.0 t);

  t


let locate (t : t) (ident : string) : Token.semantic_kind option =
  send_command t (Printf.sprintf "Locate %s." ident);
  read_until_prompt ~timeout_s:5.0 t |> parse_locate_response

let close (t : t) =
  (try send_command t "Quit." with _ -> ());
  (try close_out_noerr t.stdin with _ -> ());
  (try close_in_noerr t.out with _ -> ());
  match Unix.waitpid [Unix.WNOHANG] t.pid with
  | (0, _) ->
      (try Unix.kill t.pid Sys.sigterm with _ -> ());
      ignore (Unix.waitpid [] t.pid)
  | _ -> ()
