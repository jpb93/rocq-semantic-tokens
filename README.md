# coq-tokens

A semantic token analyzer for Coq/Rocq files. Get syntax highlighting that actually distinguishes types from constructors from functions from variables. No waiting for the proof assistant.

## The problem

Coq-lsp only knows about code it has processed. Open a file and want to read it? Too bad. Everything looks the same until you step through. This is especially painful when browsing unfamiliar code or jumping around a large development.

## The solution

Static analysis. Two passes over the token stream:

1. **Globals pass** - Find all top-level definitions, parse constructor types
2. **Scope pass** - Walk the file tracking local bindings, emit semantic tokens

It won't catch everything (notations, imports, and some edge cases need the kernel), but it handles what matters: definitions, types, constructors, and proper variable scoping including shadowing.

Is this overkill? Probably. Did I write a type expression parser just to track constructor parameter types? Yes. Do I regret it? Not yet.

## Quick start

```bash
# Build
dune build

# Run
dune exec coq_tokens -- yourfile.v

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

## Limitations

This is static analysis, not a type checker. Some things we currently can't know:

- Standard library types (would need `.glob` files or Fleche)
- Notation-defined syntax
- Anything requiring evaluation

## Future

- Hybrid mode: instant static results, Fleche ground truth as it catches up
- Read `.glob` files for dependencies
- Contribute semantic tokens upstream to coq-lsp

Or I'll get distracted by something else. We'll see.

## License

MIT
