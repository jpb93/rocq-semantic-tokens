type token_kind =
  (* Value-carrying *)
  | IDENT of string
  | KEYWORD of string
  | NUMBER of string
  | STRING of string
  (* Brackets *)
  | LPAREN | RPAREN
  | LBRACE | RBRACE
  | LBRACKET | RBRACKET
  (* Punctuation *)
  | COLON | COLONEQUAL | COLONCOLON | COLONLT | COLONGT
  | COMMA | DOT | DOTLPAREN | DOTDOT | SEMICOLON
  (* Comparison *)
  | LT | GT | LE | GE | NEQ | EQUAL
  (* Arrows *)
  | ARROW | FATARROW | LEFTARROW | BIARROW | GTARROW
  (* Logical *)
  | AMPERSAND | AMPAMP
  | PIPE | PIPEPIPE | PIPEDASH
  | SLASHBACKSLASH | BACKSLASHSLASH
  (* Arithmetic *)
  | PLUS | PLUSPLUS | MINUS | STAR | SLASH
  (* Other operators *)
  | BANG | PERCENT | QUESTION | QUESTIONEQ
  | AT | CARET | TILDE
  | BACKSLASH | HASH | HASHBRACKET
  | LTCOLON
  (* Special *)
  | UNDERSCORE
  | COMMENT
  | EOF

type token = {
  kind : token_kind;
  byte_offset : int;
  len : int; 
}
  
type semantic_kind =
  | Type
  | Constructor
  | Function
  | Module
  | Variable
  
type semantic_token = {
  pos : int;
  len : int;
  sem_kind : semantic_kind;
}

let token_kind_to_string = function
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | KEYWORD s -> "KEYWORD(" ^ s ^ ")"
  | NUMBER s -> "NUMBER(" ^ s ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | COLON -> "COLON"
  | COLONEQUAL -> "COLONEQUAL"
  | COLONCOLON -> "COLONCOLON"
  | COLONLT -> "COLONLT"
  | COLONGT -> "COLONGT"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | DOTLPAREN -> "DOTLPAREN"
  | DOTDOT -> "DOTDOT"
  | SEMICOLON -> "SEMICOLON"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"
  | NEQ -> "NEQ"
  | EQUAL -> "EQUAL"
  | LTCOLON -> "LTCOLON"
  | ARROW -> "ARROW"
  | FATARROW -> "FATARROW"
  | LEFTARROW -> "LEFTARROW"
  | BIARROW -> "BIARROW"
  | GTARROW -> "GTARROW"
  | AMPERSAND -> "AMPERSAND"
  | AMPAMP -> "AMPAMP"
  | PIPE -> "PIPE"
  | PIPEPIPE -> "PIPEPIPE"
  | PIPEDASH -> "PIPEDASH"
  | SLASHBACKSLASH -> "SLASHBACKSLASH"
  | BACKSLASHSLASH -> "BACKSLASHSLASH"
  | PLUS -> "PLUS"
  | PLUSPLUS -> "PLUSPLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | BANG -> "BANG"
  | PERCENT -> "PERCENT"
  | QUESTION -> "QUESTION"
  | QUESTIONEQ -> "QUESTIONEQ"
  | AT -> "AT"
  | CARET -> "CARET"
  | TILDE -> "TILDE"
  | BACKSLASH -> "BACKSLASH"
  | HASH -> "HASH"
  | HASHBRACKET -> "HASHBRACKET"
  | UNDERSCORE -> "UNDERSCORE"
  | COMMENT -> "COMMENT"
  | EOF -> "EOF"
  
let semantic_kind_to_string = function
  | Type -> "Type"
  | Constructor -> "Constructor"
  | Function -> "Function"
  | Module -> "Module"
  | Variable -> "Variable"
