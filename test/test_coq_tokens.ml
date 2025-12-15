open Coq_tokens

let test_type_parser () =
  let tests = [
    ("nat", "nat", false);
    ("nat -> nat", "(nat -> nat)", true);
    ("nat -> nat -> nat", "(nat -> (nat -> nat))", true);
    ("(nat -> nat) -> nat", "((nat -> nat) -> nat)", true);
    ("list nat", "(list nat)", false);
    ("(nat -> nat) -> list nat -> nat", "((nat -> nat) -> ((list nat) -> nat))", true);
  ] in
  List.iter (fun (input, expected_str, expected_is_fn) ->
    let tokens = Lexer.lex input in
    let (expr, _) = Type_parser.parse_type_expr tokens in
    let result_str = Type_expr.type_expr_to_string expr in
    let result_is_fn = Type_expr.is_function_type expr in
    if result_str <> expected_str then
      Printf.printf "FAIL: %s\n  expected: %s\n  got: %s\n" input expected_str result_str
    else if result_is_fn <> expected_is_fn then
      Printf.printf "FAIL: %s\n  is_function expected: %b got: %b\n" input expected_is_fn result_is_fn
    else
      Printf.printf "PASS: %s\n" input
  ) tests

let () = 
  print_endline "=== Type Parser Tests ===";
  test_type_parser ()
