<!-- markdownlint-disable MD013 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-parser.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-parser)
[![Javadoc](https://www.javadoc.io/badge/org.eolang/eo-parser.svg)](https://www.javadoc.io/doc/org.eolang/eo-parser)

# eo-parser

A spec-driven EO → XMIR converter. Reads `.eo` source text line by line, classifies each line per the rules in [`PARSER_SPEC.md`](PARSER_SPEC.md), and emits XMIR via [Xembly](https://www.xembly.org). No ANTLR, no intermediate AST — one in-memory state shared by classification, cross-line validation, and emission.

The public entry point is `EoSyntax`:

```java
import com.jcabi.xml.XML;
import org.cactoos.io.InputOf;
import org.eolang.parser.EoSyntax;

final XML xmir = new EoSyntax(new InputOf("[] > foo\n")).parsed();
```

## Parser pipeline

| Stage | Where |
| --- | --- |
| Line-by-line lex + classify + emit (direct EO → XMIR) | `Eo.java`, `Ln*` line shapes |
| Post-parse XSL passes (alias resolution, FQN building, etc.) | `src/main/resources/org/eolang/parser/parse/*.xsl` |
| Canonical printing (XMIR → EO round-trip) | `Xmir`, `src/main/resources/org/eolang/parser/print/*.xsl` |

## Design overview

The parser is a **single-pass line-driven state machine**. There is no AST: each source line is classified, validated against an indent stack, and emitted as Xembly directives in the same step. The XMIR is the only structured artefact.

The runtime collaboration is small — five objects, all package-private:

| Class | Role |
| --- | --- |
| `Eo` | Top-level driver. Splits input into `Span`s (one per source line), classifies each via `Eo.classify`, dispatches to the chosen `Line`. Also runs end-of-file checks (text-block closure, dangling comments) and the close-time hooks for popping levels. |
| `Line` (interface) | A single line's behaviour: `void into(Stack, Globals, Emit)`. Implemented by `LnApplication`, `LnFormation`, `LnReversed`, `LnOnlyPhi`, `LnCompactTuple`, `LnMethod`, `LnMeta`, `LnComment`, `LnBlank`, `LnTextBlock` — one class per spec line shape. Each one is the authoritative parser for its shape; classifier-level checks (e.g. `Eo.onlyPhi`) are shape *detectors* only. |
| `Stack` + `Level` | The indent stack of §5 in the spec. One `Level` per occupied indent column, carrying `kind`, `openness`, `named?`, child counters, and per-kind state (compact-tuple `N`, bare-reversed receiver flag, etc.). `Stack.popDeeperThan(indent)` fires close-time hooks (`Eo.checkOnClose`) so each level reports its own R-5.3 violation. |
| `Globals` | Cross-line transient state: pending comment block, in-flight text-block body, blank-line counter, "first object emitted" flag. Lets `Ln*` classes communicate without threading parameters through the line dispatch. |
| `Emit` | Xembly-`Directives` sink. Line shapes call `object`, `voidParam`, `method`, `star`, `close`, `set`, `slot` against this surface; `Emit` also exposes `savepoint`/`rollback` so a `ParseError` thrown inside a single line rolls back just that line's directives, allowing recovery and continued parsing. |

Below the line layer, **`Tokens`** is the per-line lexer used by every `Ln*` (head + chain + args + suffix), and **`Emissions`** centralises all `Value`-to-XMIR rendering rules — single source of truth for §9.4 emission, escape decoding (§9.7.3), and inline-phi inside paren groups (§3.10.10a). `Value`, `MethodChain`, `Suffix`, `Span` are small structural holders.

### Design choices

- **No AST.** Classify-then-emit on the same line keeps the parser short and the source-to-XMIR mapping inspectable.
- **One class per line shape.** Adding a new construct = adding one `Ln*` class and one classifier branch in `Eo`. No grammar regeneration.
- **`Stack` owns cross-line semantics.** `Ln*` classes never look at sibling lines; they only consult/mutate the top of the stack via well-defined `push`/`replace`/`pop` hooks.
- **Spec is the contract.** Every `Ln*` class header cites the §/R-rules it implements. Behaviour disagreements are bugs in one or the other — `PARSER_SPEC.md` and the code are kept in sync by deliberate audit (see the recent §3.6 / §3.10 / §9.7 reconciliations).
- **Package-private surface.** Only `EoSyntax`, `Xmir`, `StrictXmir`, `TrFull`, `ObjectName`, `OnDefault`, `OnDetailed` are exported. Everything else is an implementation detail and free to change.
- **Fail-fast at line scope.** Errors surface as `ParseError` with a `[line:col]` position; `Eo.dispatch` catches and rolls back the line's emissions, then the parser keeps going so a single malformed line doesn't poison the rest of the file.

## How to Test

All parser tests live in `org.eolang.parser.EoSyntaxTest`. The class drives four parameterized YAML directories under `src/test/resources/org/eolang/parser/`:

| Method | YAML directory | Purpose |
| --- | --- | --- |
| `validatesEoSyntax` | `eo-syntax/` | Valid EO sources — the parser must produce the asserted XMIR shape with no errors. |
| `checksEoPacks` | `eo-packs/` | End-to-end pipeline checks — XSL stylesheets listed in `sheets:` run after the parser. |
| `checksTypoPacks` | `eo-typos/` | Intentionally invalid EO — the parser must emit an error at the expected `line:` with a message containing `message:`. |
| `checksXsdMistakes` | `xsd-mistakes/` | XMIR that should fail XSD validation. |

Additional suites cover round-trip printing (`XmirTest`, `print-packs/`) and strict XMIR validation (`StrictXmirTest`).

## YAML pack format

Each YAML pack is a single document with the following top-level keys:

- `sheets` (list of strings, optional) — paths of XSL stylesheets to run after the parser, in order. Empty list means "parser output only".
- `asserts` (list of XPath strings) — XPath expressions; every one must match against the resulting XMIR.
- `input` (string) — EO source text. Use the YAML `|` block scalar so newlines and indentation are preserved.
- `line` (int, typo packs only) — the line where the expected error occurs.
- `message` (string, typo packs only) — substring the parser's error text must contain.
- `skip: true` (optional) — temporarily quarantine a pack; the test treats it as skipped (`Assumptions.assumeTrue`).

### Example: a valid-syntax pack

```yaml
sheets: []
asserts:
  - /object[not(errors)]
  - /object[@version != '']
  - //o[@base='Φ.a' and @line='3']
  - //o[@base='Φ.b-друг' and @line='4']
  - //o[@base='Φ.c4-5' and @line='5']
input: |
  +package test

  a > abc
    b-друг
      c4-5
```

### Example: a typo pack

```yaml
line: 1
message: |-
  [1:5] error: 'meta parts must be separated by exactly one space'
input: |
  +meta with  spaces
```

The parser emits errors in the canonical `[L:P] error: '<message>'` form (`MsgLocated`) followed by the offending source line and a caret pointer (`MsgUnderlined`). YAML `message:` only needs to be a substring — typically the `[L:P] error:` header is enough.

## XSL stylesheets

The `sheets:` paths are classpath-relative. Two main directories:

- `/org/eolang/parser/parse/` — runs after the parser to enrich the XMIR (e.g. `expand-aliases.xsl`, `resolve-aliases.xsl`, `build-fqns.xsl`, `add-default-package.xsl`, `wrap-method-calls.xsl`).
- `/org/eolang/parser/print/` — XMIR → EO round-trip passes used by `Xmir.toEO`.

Inside `asserts:`, the rendered XMIR uses XMIR's R-9.3 token mapping: `Q` → `Φ`, `@` → `φ`, `^` → `ρ`, `$` → `ξ`. So a source `Q.foo` appears as `@base='Φ.foo'`, not `@base='Q.foo'`.
