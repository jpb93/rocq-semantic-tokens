open Token
open Keywords

let is_digit c =
  c >= '0' && c <= '9'
  
let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_char c =
  is_ident_start c || is_digit(c) || c = '\''
  
  
let make_token kind byte_offset len : token =
  { kind; byte_offset; len }

let lex source =
  let len = String.length source in
  
  let is_at_end pos = pos >= len in
  
  let peek peek_pos = 
    if is_at_end peek_pos then None
    else Some source.[peek_pos] in
  
  let rec read_while pred pos =
    if pos < len && pred source.[pos] then
      read_while pred (pos + 1) 
    else
      pos
  in
  
  let rec skip_comment pos depth =
    if is_at_end pos then
      failwith "Unexpected end of file in comment"
    else if peek pos = Some '*' && peek (pos + 1) = Some ')' then
      if depth = 1 then pos + 2
      else skip_comment (pos + 2) (depth - 1)
    else if peek pos = Some '(' && peek (pos + 1) = Some '*' then
      skip_comment (pos + 2) (depth + 1)
    else
      skip_comment (pos + 1) depth
  in

  
  let scan pred pos =
    let end_pos = read_while pred pos in
    let substr_len = end_pos - pos in
    let s = String.sub source pos substr_len in
    (end_pos, s)
  in 
  
  let rec loop pos acc =
    if is_at_end pos then
      List.rev (make_token EOF pos 0 :: acc)
    else
     match source.[pos] with
      (* Whitespace *)
      | ' ' | '\n' | '\t' | '\r' -> loop (pos + 1) acc

      (* Comments *)
      | '(' when peek (pos + 1) = Some '*' ->
          let end_pos = skip_comment (pos + 2) 1 in
          loop end_pos acc

      (* Brackets *)
      | '(' -> loop (pos + 1) (make_token LPAREN pos 1 :: acc)
      | ')' -> loop (pos + 1) (make_token RPAREN pos 1 :: acc)
      | '{' -> loop (pos + 1) (make_token LBRACE pos 1 :: acc)
      | '}' -> loop (pos + 1) (make_token RBRACE pos 1 :: acc)
      | '[' -> loop (pos + 1) (make_token LBRACKET pos 1 :: acc)
      | ']' -> loop (pos + 1) (make_token RBRACKET pos 1 :: acc)

      (* Colon variants: := :: :< :> : *)
      | ':' when peek (pos + 1) = Some '=' ->
          loop (pos + 2) (make_token COLONEQUAL pos 2 :: acc)
      | ':' when peek (pos + 1) = Some ':' ->
          loop (pos + 2) (make_token COLONCOLON pos 2 :: acc)
      | ':' when peek (pos + 1) = Some '<' ->
          loop (pos + 2) (make_token COLONLT pos 2 :: acc)
      | ':' when peek (pos + 1) = Some '>' ->
          loop (pos + 2) (make_token COLONGT pos 2 :: acc)
      | ':' -> loop (pos + 1) (make_token COLON pos 1 :: acc)

      (* Dot variants: .( .. . *)
      | '.' when peek (pos + 1) = Some '(' ->
          loop (pos + 2) (make_token DOTLPAREN pos 2 :: acc)
      | '.' when peek (pos + 1) = Some '.' ->
          loop (pos + 2) (make_token DOTDOT pos 2 :: acc)
      | '.' -> loop (pos + 1) (make_token DOT pos 1 :: acc)

      (* Less-than variants: <-> <- <: <= <> < *)
      | '<' when peek (pos + 1) = Some '-' && peek (pos + 2) = Some '>' ->
          loop (pos + 3) (make_token BIARROW pos 3 :: acc)
      | '<' when peek (pos + 1) = Some '-' ->
          loop (pos + 2) (make_token LEFTARROW pos 2 :: acc)
      | '<' when peek (pos + 1) = Some ':' ->
          loop (pos + 2) (make_token LTCOLON pos 2 :: acc)
      | '<' when peek (pos + 1) = Some '=' ->
          loop (pos + 2) (make_token LE pos 2 :: acc)
      | '<' when peek (pos + 1) = Some '>' ->
          loop (pos + 2) (make_token NEQ pos 2 :: acc)
      | '<' -> loop (pos + 1) (make_token LT pos 1 :: acc)

      (* Greater-than variants: >-> >= > *)
      | '>' when peek (pos + 1) = Some '-' && peek (pos + 2) = Some '>' ->
          loop (pos + 3) (make_token GTARROW pos 3 :: acc)
      | '>' when peek (pos + 1) = Some '=' ->
          loop (pos + 2) (make_token GE pos 2 :: acc)
      | '>' -> loop (pos + 1) (make_token GT pos 1 :: acc)

      (* Equal variants: => = *)
      | '=' when peek (pos + 1) = Some '>' ->
          loop (pos + 2) (make_token FATARROW pos 2 :: acc)
      | '=' -> loop (pos + 1) (make_token EQUAL pos 1 :: acc)

      (* Minus/arrow: -> - *)
      | '-' when peek (pos + 1) = Some '>' ->
          loop (pos + 2) (make_token ARROW pos 2 :: acc)
      | '-' -> loop (pos + 1) (make_token MINUS pos 1 :: acc)

      (* Pipe variants: || |- | *)
      | '|' when peek (pos + 1) = Some '|' ->
          loop (pos + 2) (make_token PIPEPIPE pos 2 :: acc)
      | '|' when peek (pos + 1) = Some '-' ->
          loop (pos + 2) (make_token PIPEDASH pos 2 :: acc)
      | '|' -> loop (pos + 1) (make_token PIPE pos 1 :: acc)

      (* Ampersand variants: && & *)
      | '&' when peek (pos + 1) = Some '&' ->
          loop (pos + 2) (make_token AMPAMP pos 2 :: acc)
      | '&' -> loop (pos + 1) (make_token AMPERSAND pos 1 :: acc)

      (* Plus variants: ++ + *)
      | '+' when peek (pos + 1) = Some '+' ->
          loop (pos + 2) (make_token PLUSPLUS pos 2 :: acc)
      | '+' -> loop (pos + 1) (make_token PLUS pos 1 :: acc)

      (* Slash variants: /\ / *)
      | '/' when peek (pos + 1) = Some '\\' ->
          loop (pos + 2) (make_token SLASHBACKSLASH pos 2 :: acc)
      | '/' -> loop (pos + 1) (make_token SLASH pos 1 :: acc)

      (* Backslash variants: \/ \ *)
      | '\\' when peek (pos + 1) = Some '/' ->
          loop (pos + 2) (make_token BACKSLASHSLASH pos 2 :: acc)
      | '\\' -> loop (pos + 1) (make_token BACKSLASH pos 1 :: acc)

      (* Question variants: ?= ? *)
      | '?' when peek (pos + 1) = Some '=' ->
          loop (pos + 2) (make_token QUESTIONEQ pos 2 :: acc)
      | '?' -> loop (pos + 1) (make_token QUESTION pos 1 :: acc)

      (* Hash variants: #[ # *)
      | '#' when peek (pos + 1) = Some '[' ->
          loop (pos + 2) (make_token HASHBRACKET pos 2 :: acc)
      | '#' -> loop (pos + 1) (make_token HASH pos 1 :: acc)

      (* Simple single-char *)
      | ',' -> loop (pos + 1) (make_token COMMA pos 1 :: acc)
      | ';' -> loop (pos + 1) (make_token SEMICOLON pos 1 :: acc)
      | '*' -> loop (pos + 1) (make_token STAR pos 1 :: acc)
      | '!' -> loop (pos + 1) (make_token BANG pos 1 :: acc)
      | '%' -> loop (pos + 1) (make_token PERCENT pos 1 :: acc)
      | '@' -> loop (pos + 1) (make_token AT pos 1 :: acc)
      | '^' -> loop (pos + 1) (make_token CARET pos 1 :: acc)
      | '~' -> loop (pos + 1) (make_token TILDE pos 1 :: acc)
      | '_' -> loop (pos + 1) (make_token UNDERSCORE pos 1 :: acc)

      (* Numbers *)
      | c when is_digit c ->
          let (end_pos, s) = scan is_digit pos in
          loop end_pos (make_token (NUMBER s) pos (end_pos - pos) :: acc)

      (* Identifiers and keywords *)
      | c when is_ident_start c ->
          let (end_pos, s) = scan is_ident_char pos in
          let tok = if List.mem s keywords then KEYWORD s else IDENT s in
          loop end_pos (make_token tok pos (end_pos - pos) :: acc)

      (* Unknown *)
      | c -> failwith (Printf.sprintf "Unknown character '%c' at position %d" c pos)
   in 
   
   loop 0 []
