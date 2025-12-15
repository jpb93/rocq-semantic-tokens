open Token

let rec parse_type_expr (tokens : token list) : Type_expr.t * token list =
  let (left, rest) = parse_app_expr tokens in
  match rest with
  | { kind = ARROW; _} :: rest2 ->
      (* right-associative, therefore we parse 'rest2' as another type expr*)
      let (right, rest3) = parse_type_expr rest2 in
      (Arrow (left, right), rest3)
  | _ -> (left, rest)

  and parse_app_expr (tokens : token list) : Type_expr.t * token list =
    (* parse the first atom *)
    let (first, rest) = parse_atom tokens in
    parse_app_rest first rest
    
  and parse_app_rest (acc: Type_expr.t) (tokens : token list) : Type_expr.t * token list =
    match tokens with
    | { kind = IDENT _; _} :: _
    | { kind = LPAREN; _} :: _ ->
      let (next, rest) = parse_atom tokens in
      parse_app_rest (App (acc, next)) rest
    | _ -> (acc, tokens)
    
  and parse_atom (tokens : token list) : Type_expr.t * token list =
    match tokens with
    | { kind = IDENT s; _ } :: rest -> (Ident s, rest)
    | { kind = LPAREN; _ } :: rest -> 
      let (inner, rest2) = parse_type_expr rest in
      (match rest2 with
      | { kind = RPAREN; _ } :: rest3 -> (inner, rest3)
      | _ -> failwith "Expected closing paren")
    | _ -> failwith "Expected identifier or '('"

