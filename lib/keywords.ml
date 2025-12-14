let reserved_keywords = [
  "_"; "as"; "at"; "cofix"; "else"; "end"; "exists"; "exists2"; "fix"; "for";
  "forall"; "fun"; "if"; "in"; "let"; "match"; "mod"; "Prop"; "return";
  "Set"; "then"; "Type"; "using"; "where"; "with"
]

(* The Vernacular *)

let assertion_keywords = [
  "Theorem"; "Lemma"; "Remark"; "Fact"; "Corollary"; "Proposition";
  "Definition"; "Example"
]

let assumption_keywords = [
  "Axiom"; "Conjecture"; "Parameter"; "Parameters";
  "Variable"; "Variables"; "Hypothesis"; "Hypotheses"
]

let proof_keywords = [
  "Proof"; "Qed"; "Defined"; "Admitted"
]

let binding_keywords = [
  "Fixpoint"; "Inductive"; "CoInductive";
]

let structure_keywords = [
  "Record"; "Structure"; "Class"; "Instance";
  "Module"; "Section"; "End";
  "Require"; "Import"; "Export"; "Open"; "Scope"
]

let function_definining_keywords =
  assertion_keywords
  @ assertion_keywords
  @ ["Fixpoint"; "Instance"]

let type_defining_keywords = 
  ["Inductive"; "CoInductive"; "Record"; "Structure"; "Class"]


let keywords = reserved_keywords 
  @ assertion_keywords 
  @ assumption_keywords
  @ proof_keywords
  @ binding_keywords
  @ structure_keywords
