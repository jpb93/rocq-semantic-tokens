# coq-tokens

A semantic token analyzer for Coq/Rocq files. Get syntax highlighting that actually distinguishes types from constructors from functions from variables. No waiting for the proof assistant.

## The problem

Coq-lsp only knows about code it has processed. Open a file and want to read it? Too bad. Everything looks the same until you step through. This is especially painful when browsing unfamiliar code or jumping around a large development.

## The solution

Static analysis. Two passes over the token stream:

1. **Globals pass** - Find all top-level definitions, parse constructor types
2. **Scope pass** - Walk the file tracking local bindings, emit semantic tokens

For symbols not defined in the file (stdlib, imports), use `--query` to resolve them via [proof-archivist](https://gitlab.com/cogumbreiro/proof-archivist/).

## Quick start

```bash
# Build
dune build

# Run (static analysis only)
dune exec coq_tokens -- yourfile.v

# Run with stdlib symbol resolution
dune exec coq_tokens -- --query yourfile.v

# Or via make
make run FILE=yourfile.v
```

Output format:
```
position:length kind "text"
```

Example on a file with shadowed bindings:
```
11:7 Function "default"
49:7 Variable "default"
73:7 Variable "default"
```

## How it works

```
Source (.v)
    │
    ▼
┌─────────┐
│  Lexer  │  tokenizes into IDENT, KEYWORD, operators, etc.
└─────────┘
    │
    ▼
┌─────────┐
│ Globals │  collects top-level definitions
└─────────┘  parses constructor parameter types
    │
    ▼
┌─────────┐
│  Scope  │  walks tokens, tracks local bindings
└─────────┘  emits semantic tokens with resolved kinds
    │
    ▼
┌─────────┐
│ Query   │  (optional) resolves unknown symbols via proof-query
└─────────┘
    │
    ▼
 Output
```

The interesting bit is constructor parameter tracking. When we see:

```coq
Inductive wrapper := Wrap : (nat -> nat) -> wrapper.
```

We record that `Wrap` takes one parameter of type `nat -> nat`. Later, in a match:

```coq
match w with
| Wrap f => f x
end
```

We know `f` should be highlighted as a function, not a plain variable.

Was this necessary? No. But it bothered me.

## Token kinds

| Kind | Examples |
|------|----------|
| Type | `nat`, `list`, `bool` |
| Constructor | `S`, `O`, `Some`, `None`, `Wrap` |
| Function | `plus`, `map`, `plus_O_n` |
| Module | `Nat`, `List` |
| Variable | params, let bindings, match bindings |
| Ltac | `intros`, `reflexivity`, `rewrite` |

## Optional: Resolving stdlib symbols

By default, coq-tokens only knows about definitions in the current file. To resolve standard library symbols (like `nat`, `S`, `O`, `list`, etc.), use the `--query` flag:

```bash
dune exec coq_tokens -- --query yourfile.v
```

This requires [proof-archivist](https://gitlab.com/cogumbreiro/proof-archivist/) to be installed:

```bash
# Clone and build proof-archivist
git clone https://gitlab.com/cogumbreiro/proof-archivist/
cd proof-archivist
opam install coq-lsp alcotest  # dependencies
dune build
dune install
```

The `--query` flag batches all unknown symbols into a single call to `proof-query locate`, making it fast even with many unknowns.

## Limitations

This is static analysis, not a type checker. Some things we can't know without `--query`:

- Standard library types and constructors
- Symbols from imported modules
- Notation-defined syntax
- Anything requiring evaluation

## Future

- Hybrid mode: instant static results, Fleche ground truth as it catches up
- Read `.glob` files for dependencies
- Contribute semantic tokens upstream to coq-lsp

Or I'll get distracted by something else. We'll see.

## Acknowledgments

Symbol resolution via `--query` is powered by [proof-archivist](https://gitlab.com/cogumbreiro/proof-archivist/) by [Tiago Cogumbreiro](https://cogumbreiro.github.io/). The `proof-query locate` command provides fast, batched symbol lookup through coq-lsp's petanque interface.

## License

MIT
