<!-- markdownlint-disable MD007 MD012 MD013 MD032 MD038 MD040 MD043 -->

# EO Parser Specification

This document is the complete contract for an EO parser. It defines the input language (EO source code), the output language (XMIR), and every rule that turns one into the other. The spec is self-sufficient: an implementer can build a conforming parser from this document alone.

The spec is written from simple constructs to complex; each section builds on terminology and rules established earlier.

---

## 0. Background and conventions

### 0.1 EO

**EO** is an object-oriented programming language with significant whitespace. A program is a tree of *objects*; every object has zero or more *named attributes*. Indentation determines nesting: deeper indent means a child of the previous shallower line. Objects are declared inside *formations* (templates with named parameters) and combined by *application* (passing arguments to a head expression) and *method dispatch* (calling a named attribute of an object). Two notations exist for both operations — a one-line *horizontal* form and a multi-line *vertical* form.

### 0.2 XMIR

**XMIR** (XML-based meta-representation of EO) is the parser's output format. An XMIR document is an XML tree whose every node is `<o>` and whose attributes carry the object's name, base reference, position, etc. The complete XMIR schema is governed by an external XSD; this spec covers only the surface aspects the parser must emit.

Special XMIR markers used in this spec:

| Marker | Meaning |
| --- | --- |
| `<o>` | A single EO object node. Carries `name`, `base`, line/column attributes. |
| `<part>` | Sub-element of a meta directive (§3.2). |
| `@as='name'` | Attribute on an `<o>` indicating which slot of the parent it fills (used for inline bindings, §3.12). |
| `@star` | Attribute marking a tuple of vertical children (§3.9). |
| `Φ` | The XMIR root namespace marker — the global namespace of all objects. The source-level `Q` token is promoted to `Φ` in XMIR output. |
| `/object/errors/error` | Where the parser writes recoverable errors (§7). |

Each line's classification (§3) maps to one or more `<o>` emissions, and the indent stack (§5) controls when nested objects open and close. Concrete emission recipes per outer kind are described per construct in §3 and §4; the formal XMIR schema below this surface contract is normative for downstream consumers.

#### External XMIR references

The following documents describe XMIR beyond this spec. They are not required to implement the parser, but are normative for the XMIR format the parser produces and useful when verifying emitted output:

- **XSD schema** — `https://www.eolang.org/XMIR.xsd`. The XML schema all conforming XMIR must validate against.
- **XMIR HTML specification** — `https://www.eolang.org/XMIR.html`. Human-readable description of XMIR element/attribute semantics.
- **XMIR introduction** — `https://news.eolang.org/2022-11-25-xmir-guide.html`. Background on why XMIR exists and how it sits between parsing and code generation.

If this spec and the XSD ever disagree on the shape of emitted XMIR, the XSD wins — this spec describes *what the parser does*; the XSD defines *what valid XMIR is*.

### 0.3 The parser model

The parser is a single-pass, direct EO → XMIR converter. There is no intermediate abstract syntax tree: each source line emits XMIR as it is recognised. The classifier, the cross-line validator, and the emitter share one in-memory data structure — the *indent stack* defined in §5.

### 0.4 Reading this document

- Every rule has a numbered identifier (`R-N.M`) for cross-reference.
- Code shown in fenced blocks is EO source. Where a snippet is illegal, the comment `← rejected` marks the failing line.
- The phrase "the parser must" describes hard constraints. "Should" describes recommended behavior an implementation may relax.
- §1 establishes terminology used everywhere else. Read it first.

### 0.5 Performance and memory characteristics

A conforming parser meets these complexity bounds:

- **Time:** O(n) in the number of source lines (single pass). Per-line work is O(L) in line length for classification and emission; total: O(N) in source character count.
- **Memory:** O(D) for the indent stack (§5.1), where D is the maximum indent depth in the source. O(B) for any open BYTES continuation (§3.13) or TEXT block (§3.11), where B is body size. O(C) for the pending comment buffer (§5.1.1), where C is the largest comment block.
- **No backtracking:** the cross-line FSM (§5.2) consults only the current stack top and a small global state; no rewriting of earlier emission is required after a line is processed (modulo the per-line savepoint for error recovery, §7.2).
- **Pathological inputs:** deeply nested formations grow the indent stack linearly with depth; no superlinear blowup. Long `.method` chains emit O(K) flat siblings for K chain links (§9.0.3).

---

## 1. Terminology

The terms below are used precisely throughout this document.

### 1.1 Source structure

- **Source text** — the input character stream.
- **Line** — a maximal substring of the source that contains no line terminator. Lines are numbered from 1.
- **Blank line** — a line whose contents are entirely whitespace (or empty).
- **Comment line** — a line whose first non-space character is `#`.
- **Indent** — the count of leading spaces of a non-blank line. Must be even (§2.2).
- **Indent level** — `indent / 2`. The top-level indent level is 0.

### 1.2 Objects and expressions

- **Object** — the EO unit of meaning. Compiles to one XMIR `<o>` element.
- **Expression** — the syntactic form that introduces an object. May span one or more lines.
- **Single-line expression** — an expression occupying exactly one line.
- **Multi-line expression** — an expression spanning a head line and a block of deeper-indented children, possibly continued by same-indent `.method` lines.
- **Sibling expressions** — expressions sharing the same indent and the same immediate parent.
- **Outer kind** of an expression — the kind of its outermost AST node. The complete enumeration of outer kinds is given in §4 and summarised in Appendix A.
- **Naming line** of an expression — the source line on which the expression's name suffix is required to live. See §6.2 for the per-kind mapping.
- **Formation** — an EO object that introduces a new namespace of named attributes. Syntactic form: a head line opening with `[` listing zero or more *void parameters*, optionally followed by a body block of child expressions at the next indent. A formation may carry a name suffix (§1.4). When the suffix is `> name /sig`, the formation is also an **atom** (§6.3). The shorthand `> [params] >` mid-line introduces an **inline-phi formation** (§4.5).
- **Void** — a parameter slot of a formation. Has a name but no value at declaration time; the value is supplied by the caller.

### 1.3 Children of a formation

A formation's body block contains child expressions. We distinguish two roles based on syntactic shape:

- **Master child** — a child expression whose outer kind is `bare-formation`, `inline-phi-formation`, or whose name suffix is `/sig` (atom). I.e., children whose role is self-evident from the brackets (`[` head or `> [..] >` mid-line marker). A master child may carry an optional name suffix; it may be preceded by zero or one blank line, and when its name suffix is a `+>` test attribute the blank line is mandatory (§6.5).
- **Plain child** — every other child expression: `head`, `hmethod`, `happlication`, `vapplication`, `vmethod`, `bare-reversed`, `reversed-with-hargs`, `compact-tuple`, or `text-block`. A plain child **must** carry a name suffix on its naming line (§6.2). A plain child may not be preceded by a blank line.

The distinction is structural: master children open a sub-formation visible in the source via brackets, so a name is optional. Plain children would be visually indistinguishable from siblings without an explicit name suffix, so one is required.

### 1.4 Name suffixes

Four base forms (mutually exclusive on any given line) plus their modifiers; §3.10 is authoritative. Reference enumeration:

- **`> name`** — explicit name binding. May carry modifiers: `!` (const) and `/sig` (atom signature). The modifiers are not separate forms — they decorate the `> name` base.
- **`>>`** — auto-generated name. May carry `!` (const). `/sig` is forbidden on `>>` (R-3.10.2).
- **`+> name`** — truthy test attribute (§6.3). `name` must be a `NAME` token.
- **`-> name`** — throwing test attribute (§6.3): the test is expected to throw an exception. Parallel to `+>` in every respect (same `NAME`-token rule, same depth constraint R-6.3.3, same mandatory blank line R-6.5.3); only the emitted marker prefix differs (`-` instead of `+`, §9.4).
- **(none)** — no suffix. Legal except when the object is a plain child of a formation (§6.2).

A composite inline-phi suffix `> [params] > name` (or `> [params] >>`) introduces an inline-phi formation as the line's outer kind (§3.10, §4.5). This is not a separate base form — it embeds one of the base forms on its right side.

### 1.5 The indent stack

The data structure the parser maintains across lines. Defined in full in §5. Briefly: a stack of *level records*, one per occupied indent. Each record carries `(indent, outer kind, openness, parent kind, named?)` and is updated on each line.

### 1.6 Openness

A property of a level record. One of:

- **`open`** — the expression may still receive deeper-indent children or a same-indent `.method` continuation.
- **`vertical-completed`** — the expression's child block has ended; a same-indent `.method` may still wrap it, but no more vertical args may be added.
- **`horizontal-completed`** — the expression cannot be extended in any direction.

---

## 2. Lexical layer

### 2.1 Encoding and line endings

R-2.1.1. Source is UTF-8.
R-2.1.2. Line endings are `\n` or `\r\n`. The parser normalises both to `\n` internally.

### 2.2 Indentation

R-2.2.1. The indent unit is **two spaces**. Odd leading-space counts are an error: `unexpected odd indent`.
R-2.2.2. Between two consecutive non-blank lines, indent may **increase by at most one level** (i.e., +2 spaces).
R-2.2.3. Indent may decrease by any amount.
R-2.2.4. Tabs in leading whitespace are rejected.

Example:

```
[] > foo
  a > x          ← indent 2 (level 1), parent foo at indent 0
    b           ← indent 4 (level 2), one level deeper than parent
      c         ← indent 6 (level 3)
  d > y          ← decrease from 6 to 2, allowed
```

Illegal:

```
[] > foo
  a > x
      b         ← rejected: indent jumped from 2 to 6 (skipping 4)
```

### 2.3 Tokens

The parser recognises the following lexical tokens:

| Token | Description / pattern |
| --- | --- |
| `META` | `+` `NAME` followed by zero or more space-separated parts; each part is one or more non-whitespace characters. Parts may contain `:`, `.`, `-`, `/`, e.g. `+rt jvm a.b.c:lib:1.0.0`. |
| `COMMENTARY` | `#` followed by the rest of the line. |
| `NAME` | `[a-z]` followed by characters other than space, line break, tab, `,`, `.`, `\|`, `'`, `:`, `;`, `!`, `?`, `]`, `[`, `}`, `{`, `)`, `(`, `🌵`. |
| `PHI` | `@` |
| `RHO` | `^` |
| `ROOT` | `Q` |
| `XI` | `$` |
| `TERM` | `T` — the bottom term (§9.3), similar to `⊥` in 𝜑-calculus. A self-contained single-character token; carries no arguments and no chain. |
| `SELF` | `%` — self-reference (§3.15). Sugar for the auto-name of the enclosing anonymous (`>>`-named) formation; substituted at compile time. A value: it may carry arguments (`% 5`) and a `.method` chain, like any other head. |
| `VOID` | `?` — the vertical-void marker (§3.4). A `? > name` body line declares a void attribute, equivalent to listing `name` in `[…]`. |
| `QDOT` | `?.` — the fragile-dispatch operator (§3.5). Accepted in every position the plain `.` dispatch is, recorded as `@fragile` in XMIR. A `?` immediately followed by `.` is `QDOT`; a `?` followed by space (`? > name`) is `VOID`. |
| `INT` | optional sign, then `0` or non-zero digit string. |
| `FLOAT` | optional sign, digits, `.`, digits, optional exponent. |
| `HEX` | `0x` followed by hex digits. |
| `STRING` | `"..."` with standard escape sequences. |
| `BYTES` | one of: `--` (empty); a single byte followed by `-`; a sequence `BB-BB(-BB)*` optionally continued across lines via trailing `-` + newline (§3.13). |
| `TEXT` | triple-quoted text block (§3.11). |
| `STAR` | `*` |
| `ARROW` | `>` |
| `DOT` | `.` |
| `COLON` | `:` |
| `SLASH` | `/` |
| `CONST` | `!` |
| `LSQ` / `RSQ` | `[` / `]` |
| `LB` / `RB` | `(` / `)` |

Indent is read directly from line leading-whitespace by the classifier (§3); there is no separate INDENT/DEDENT lexer phase.

---

## 3. Per-line classification

Each non-blank, non-comment line is classified into exactly one shape, determined by the first non-space character (after the indent) and a small lookahead.

### 3.1 Classification table

| First char (after indent) | Lookahead | Line shape |
| --- | --- | --- |
| `+` | followed by digit | signed-number application (see §3.6) |
| `+` | followed by `+>` | test-attribute shorthand — `++> name` desugars to `[] +> name` (§3.4 / R-6.3.6) |
| `+` | otherwise | meta directive (§3.2) |
| `-` | followed by `->` | throwing-test-attribute shorthand — `--> name` desugars to `[] -> name` (§3.4 / R-6.3.6) |
| `-` | otherwise | application starting with literal — signed number, `--` empty bytes, etc. (§3.6) |
| `#` | — | comment (§3.3) |
| `.` | — | method-dispatch line (§3.5) |
| `\|` | — | pipe-application line (§3.14) |
| `[` | — | formation line (§3.4) |
| `"""` (start of line, no other content) | — | text-block opener (§3.11) |
| identifier | ends with `.` followed by space/EOL | reversed-dispatch line (§3.8) |
| identifier | otherwise | application or just-an-identifier (§3.6) |
| literal (`"…"`, `42`, `--`, `AB-CD`, `0xFF`, signed number) | — | application starting with literal (§3.6) |
| `*` | — | star tuple as application head (§3.6) |
| `(` | — | application starting with group (§3.6) |
| `%` | — | self-reference as application head (§3.15) |

The classifier emits a **line-shape record**:

```
LineShape {
  indent: int
  kind: enum { Meta, Comment, Blank, Formation, Application, MethodDispatch,
               ReversedDispatch, InlinePhi, TextBlockOpen, TextBlockBody,
               TextBlockClose }
  outer_kind: OuterKind     // see §4 / Appendix A
  has_name_suffix: bool
  name_suffix: NameForm     // > name | >> | +> name | -> name | > name /sig | none
  has_horizontal_args: bool
  horizontal_arg_count: int
  has_inline_phi: bool      // line ends with `> [...] >`
  has_compact_tuple: bool
  compact_tuple_n: int      // -1 means absent; 0+ valid
  // ... raw token slices for the emitter
}
```

This record is the only artefact passed from the classifier to the validator (§5) and the emitter. It contains everything the cross-line rules need.

### 3.2 Meta directive — `+name [parts]`

R-3.2.1. Legal only at indent 0.
R-3.2.2. Legal only **before** any non-meta object has been emitted.
R-3.2.3. Each space-separated token after the `+name` becomes a `<part>` element. A leading `Q` in any part is promoted to `Φ`.
R-3.2.4. At most one space between parts.
R-3.2.5. `+` followed by a digit is **not** a meta (see §3.6 — it's a signed-number literal). If the digits do not form a valid `INT` or `FLOAT` token per §9.8 (e.g., `+1foo` where the digit-run is followed by an identifier-letter with no intervening separator), the lexer rejects with a lexical error; **the parser does not silently fall back to interpreting the line as a meta**.

```
+architect yegor256@gmail.com         ← meta, legal at top
+version 0.0.0                        ← meta
+1.5                                  ← NOT a meta, it's a number literal
```

### 3.3 Comment — `# text`

A program may carry at most one comment block, and only on top of the file, before all metas and objects (issue #3993). It documents the whole program. Every other comment — indented, after a meta, after an object, or a second block lower down — is rejected. Objects are expected to be self-explanatory; per-object documentation is generated later by external tooling, not written by hand.

R-3.3.1. Leading-space count must be even (derived from R-2.2.1, which applies to every non-blank line including comments — restated here for readability).
R-3.3.2. The top comment block is a contiguous run of `#` lines at indent 0 on top of the file, before all metas and objects. It collects into a single **comment block** documenting the program (§6.4).
R-3.3.3. Inside the comment block, every line shares indent 0.
R-3.3.4. No blank line inside the comment block (§6.5.1).
R-3.3.5. At most one blank line may precede the comment block (§6.5.3).
R-3.3.6. A comment is allowed only in the top block. A comment that is indented, follows a meta, follows an object, or opens a second block is rejected: error "comment is allowed only on top of the file, before metas".
R-3.3.7. Exactly one blank line must separate the comment block from the first meta or object (§6.5.2). A block immediately followed by a meta or object, with no blank in between, is rejected: error "a blank line must separate the top comment block from the rest of the file".

```
# This is the file-level comment.
# Continued on the next line.

+package foo
+version 1.0.0

[] > foo
  # rejected — comments are allowed only on top of the file, before metas
  [] > bar
```

### 3.4 Formation — `[params] [> name [/sig]]`

R-3.4.1. Each parameter becomes a void child of the formation.
R-3.4.2. The standalone `@` parameter maps to `φ`. **Lexer note:** the §2.3 `NAME` token excludes `@`, so the `@` inside `[…]` is lexed as the `PHI` token, not as `NAME`. The void parameter list therefore accepts the token sequence `(NAME | PHI) (SPACE (NAME | PHI))*`, not just `NAME`.
R-3.4.3. `^` (`RHO` token) is rejected as a parameter name; only `NAME` and `PHI` are admitted (see lexer note in R-3.4.2).
R-3.4.4. No leading/trailing space inside `[ ]`.
R-3.4.5. No double space between parameter names.
R-3.4.6. The formation line may end with one of the optional name suffixes (§3.10).
R-3.4.7. A void attribute may also be declared **vertically** as a `? > name` body line of the formation (the `?` is the `VOID` token of §2.3). It is equivalent to listing `name` among the bracket parameters: it emits the same void child (§9.4), and `move-voids-up` hoists it among the head voids in source order, so `[name] > foo` with body lines `? > bar` and `? > test` is identical in XMIR to `[name bar test] > foo`. The `>>` auto-name suffix (§3.10) is also accepted: `? >> name` declares a void whose external `@name` is an auto-generated cactus name (§9.2, unreachable from outside), while `name` is a *file-local handle* (R-3.10.12) usable within the same `.eo` file; a bare `? >>` is a void with an auto-generated name and no handle. Filling stays positional, so the auto-generated external name does not affect how callers bind the void. `? > name` and `? >> name`, each optionally followed by one type annotation (§3.4.8), are the **only** shapes the `?` marker may take: the marker is not a value, so it may not appear as an argument (`foo ? bar`), a method receiver (`?.read`), or a reversed-dispatch argument (`foo. ? q`), and a bare `?` or any other trailing tokens are an error. The form requires a name (or auto-name) suffix and is legal only as a direct child of a formation, which has no children of its own (a deeper-indent line under a void is rejected). A `!` const marker on a void is rejected (`a void attribute must be written as ? > name or ? >> name`); a type annotation, however, is permitted inside an atom (§3.4.8). Reverse printing canonicalises every void to the bracket form, since the two are indistinguishable in XMIR.
R-3.4.8. **Type annotations on voids (atom-only).** Inside an **atom** (a formation whose head carries `/sig`), a vertical void may carry **exactly one** type annotation. Both forms below are optional; an unannotated void is left untyped (its type is inferred). Outside an atom either form is an error (`a void type annotation is allowed only inside an atom`), and the two forms are mutually exclusive on one void (`a void attribute may carry at most one type annotation`).

```
void-anno ::=  '/' type '?'?                    (own type; trailing ? = maybe-⊥)
            |  '/{' type (SPACE type)* '}'       (callback arg types; no ?)
type      ::=  type-var | NAME ('.' NAME)* | 'Q' ('.' NAME)+
```

- **Own type — `? > name /type`.** `type` is a `type-var` (§3.10.10) or a concrete forma, with an optional trailing `?` marking a **maybe-⊥** value. Emits a `@type` attribute on the void's `<o>`. A `type-var` is emitted verbatim; a concrete forma has its leading `Q.` promoted to `Φ.` at parse (R-9.3) and is later homed by `add-default-package`, with any trailing `?` preserved. Examples: `? > value /A?` emits `<o name='value' base='∅' type='A?'/>`; `? > s /Q.string?` emits `type='Φ.string?'`.
- **Callback argument types — `? > name /{type …}`.** The void is itself a formation the atom fills; the brace list gives the types of *its* void parameters — equivalently, the arguments the atom supplies to that branch. Each member is a `type-var` or a concrete forma, single-space separated, at least one required; the optional marker `?` is **not** allowed inside the list (`? is not allowed inside a /{…} argument list`). Emits an `@args` attribute, members promoted or verbatim per the split. Example: `? > not-found /{string io.file-error}` emits `<o name='not-found' base='∅' args='Φ.string io.file-error'/>`; `? > cant /{Q.string B}` emits `args='Φ.string B'`. (The type-checker treats the callback's result as the atom's own return type; the parser records only `@args`.)

Every `type-var` in an atom — the return signature, a `@type`, or an `@args` member — denotes the same variable when spelled with the same letter, universally quantified over that atom (§3.10.10). So `B` in `cant /{Q.string B}` is the `B` of `? > y /B`.
R-3.4.9. Vertical voids must stay **on top**: every `? > name` line must precede all non-void attributes of the formation body. A void that follows a non-void child — or sits between non-void children — is an error (`a void attribute must be declared above all other attributes`). For example,

```
[] > foo
  6 > six
  ? > x
```

is rejected, whereas `? > x` above `6 > six` is accepted. (Bracket-head voids are always above the body, so the rule constrains only the relative order of body lines.)

Outer kind: **`bare-formation`** (master; openness `open` for body).

```
[a b] > foo                           ← formation with two voids, named foo
[] > bar                              ← formation with no voids, named bar
[a b] > foo /number                   ← atom declaration (§6.3)
```

### 3.5 Method dispatch line — `.name [args] [> name]`

A line that begins with `.` is a *method-dispatch line*. The classifier produces line shape `MethodDispatch` regardless of how the cross-line machinery later promotes it (§5.2.3 extends it into `vmethod` or `vmethod-with-hargs`).

R-3.5.1. Leading `.` consumes the dot. The position recorded for the dispatch (`@pos`) aligns with the `.` itself, not with the method name (§9.1.3).
R-3.5.2. Name mapping inside the dispatch: `@` → `φ`, `^` → `ρ`. (Cross-references §9.3.)
R-3.5.3. Horizontal arguments are accepted after the method name; they become args of the method.
R-3.5.3a. **Fragile dispatch `?.`.** Anywhere the plain `.` dispatch operator is accepted, the fragile operator `?.` (`QDOT`, §2.3) is equally accepted: a horizontal chain link (`x?.read`, mixed `a.b?.c`), a `?.method` continuation line, and a reversed dispatch (`name?. …`). It parses identically to `.` and emits the same `<o>` — the only difference is an added `@fragile=''` marker on that link (§9.4). The operator is **syntax only** in this revision: the parser does not require `?.` for any receiver nor forbid `.`, because fragility is not known at parse time; enforcement (a fragile object must use `?.`) is a separate concern that reads the `@fragile` marker downstream. A `?` that is not immediately followed by `.` is the vertical-void marker (§3.4), not a dispatch.
R-3.5.3b. **Fragile-dispatch enforcement (warning).** A first, purely syntactic slice of the enforcement that R-3.5.3a deferred: when a regular `.` dispatch is performed **directly** on a fragile `?.` dispatch that was not applied, a `warning`-severity `<error check='fragile-dispatch'>` is reported (no type inference — only the in-chain case decidable from the `@fragile` markers is checked). `x?.y.a` warns (write `x?.y?.a`); `x?.y?.a` is clean; `(x?.y 1 2).a` is clean because the dispatch is on the result of an application, not directly on `?.y`. Structurally the offending link is a method dispatch (`@method`, no `@fragile`) whose immediately-preceding sibling is a *childless* fragile link — an application carries arg children and so does not match. The check runs before `wrap-method-calls` (on the flat chain) and covers horizontal, vertical, and reversed dispatch uniformly; the broader "object known to be fragile" case needs type inference and is out of scope.
R-3.5.4. **Cross-line ownership of standalone-`.method` rejection.** A `.method` line carries an optional name suffix; the line's *legality* is decided cross-line, not per-line. Three rules own the rejection paths:
  - **R-5.2.3(b)** — same-indent `.method` after a horizontally-completed predecessor.
  - **R-5.2.5** — `.method` as a deeper-indent line (no previous sibling at this indent).
  - **R-5.2.10** — `.method` at top level (empty stack).

  This rule exists in §3.5 only to point readers at those three locations from the per-line side of the spec.

Cross-line behaviour: covered in §5.2.3. The outer kind a `MethodDispatch` line ultimately becomes is decided there:

- 0 horizontal args on the line → top's kind becomes `vmethod` (still open for more continuations).
- ≥1 horizontal args → top's kind becomes `vmethod-with-hargs` (chain closes, horizontally-completed).

### 3.6 Application — `head [arg…] [> name]`

The largest single-line construct. `head` is one of:

- A paren group `( … )`, optionally followed by chained `.method.method` segments.
- A data literal (`"…"`, `42`, `--`, `AB-CD`, `0xFF`, signed number, hex), optionally followed by chained `.method.method` segments (e.g. `42.as-bytes`, `01-.eq x`).
- A `*` star tuple, optionally followed by chained `.method.method` segments (e.g. `*.with 1` — chains on `*` dispatch against the empty tuple object after the standard `*` → `Φ.tuple star=""` head emission of R-9.4.2).
- A bare identifier with optional chained `.method.method` segments.

After the head, zero or more space-separated arguments may follow. Each argument is itself a small dispatch:

- A group `( … )` (the parens are redundant unless the group contains more than one token or is followed by `.method`).
- A data literal with its method chain (e.g., `42.as-bytes` is one argument).
- A `*` tuple, optionally with its method chain.
- An identifier head with optional method chain.

R-3.6.1. Argument separation is by single space.
R-3.6.2. Inline binding `:label` or `:N` may follow an argument (§6.6).
R-3.6.3. All-or-nothing binding rule (§6.6.2) applies to the argument list.
R-3.6.4. The shape detector that classifies the line ignores characters inside parens and string literals — only top-level spaces and dots count.
R-3.6.5. **Horizontal formations are not arguments.** A `[params]`-headed expression is legal as a *top-of-line construct* (a `bare-formation` line, §3.4) — which includes appearing **as a vertical argument at deeper indent under another expression**. It is also legal as the inline-phi parameter list (`> [params] >`, §3.10). What is **rejected** is the horizontal anonym form: `[x]` (or `[x] body`) as a *horizontal* argument token inside another expression — typically wrapped in parens. The current ANTLR grammar admits the horizontal form via `scope : LB (happlication | hanonym) RB`; the new spec deliberately removes it.

Legal — formation as a vertical arg:

```
foo
  [x]                        ← legal: formation occupies its own line at deeper indent (vapp arg)
  5
```

Illegal — formation as a horizontal arg:

```
foo ([x] body)               ← rejected: horizontal formation as argument
foo [x] 5                    ← rejected: `[x]` in the horizontal arg list of foo
```

Outer kinds produced:
- Head only, no args, no chained `.method` → **`head`**. A `*` token alone in head position is also `head` kind, emitted with `@base='Φ.tuple'` and `@star=''` (see §9.4.2 "Star tuple as head"). Openness and wrappability are the same as any other `head`.
- Head with chained `.method.method…` but 0 horizontal args → **`hmethod`**. The head of the chain may itself be a paren group, a literal, `*`, or an identifier; the chain wraps it.
- Head + ≥1 horizontal arg → **`happlication`**. This subsumes "chain + args" — the chained head is *inside* the happlication, but the outer kind is happlication, not hmethod.
- Identifier-with-trailing-dot at line start (reversed dispatch) → covered by §3.8.

```
foo                                   ← head only
foo.bar.baz                           ← hmethod, 0 args
foo a b c                             ← happlication, 3 args
foo.bar 42 "x"                        ← happlication (head is hmethod, 2 args)
(foo bar).baz x                       ← happlication (head is hmethod over a group, 1 arg)
```

### 3.7 (Reserved)

This section number is reserved; the method-dispatch line continuation rules previously held here are folded into §3.5. Cross-references to §3.7 elsewhere in this spec resolve to §3.5.

### 3.8 Reversed dispatch — `name. [args] [> name | >> | +> name]`

The trailing-dot prefix-notation form.

R-3.8.1. The identifier ends with a trailing `.` followed by space or end-of-line. **The identifier preceding the trailing dot must be a single `NAME`, `PHI` (`@`), or `RHO` (`^`) token** — no dotted paths and no `ROOT`/`XI`/literal-rooted prefixes. Chained heads (`Q.x.foo.`) are not valid as the lead of a reversed dispatch — a trailing dot after `Q.x.foo` does not produce a reversed-dispatch line and is reported as a lexical/parse error.
R-3.8.2. **Horizontal form** — `name. arg1 arg2 …`:
  - `arg1` is the **receiver** (the dispatch target of `.name`).
  - `arg2…` are method arguments of `.name`.
  - No vertical continuation under this line (closed for deeper-indent children).
  - Inline bindings on args follow §6.6, with the receiver-exclusion clause (§6.6.3).

R-3.8.3. **Vertical form** — `name.` with no horizontal args:
  - The next non-blank line must be at exactly `indent + 2` and **must not begin with `.`**.
  - That first deeper line is the receiver.
  - Subsequent same-indent siblings (at `indent + 2`) are method args of `.name`.
  - The line itself is closed for same-indent `.method` continuation until its child block ends.

R-3.8.4. The horizontal and vertical forms are **mutually exclusive** — no mixing on the same dispatch.

R-3.8.5. **Compact tuple suffix** `*N` may follow the dot only in the vertical form (§3.9). For reversed dispatch with `*N`, `N ≥ 1` is required (the receiver occupies the first direct slot).

Outer kinds produced:
- Vertical form, no compact tuple → **`bare-reversed`**.
- Horizontal form (with hargs) → **`reversed-with-hargs`**.
- Vertical form with compact tuple → **`compact-tuple`** (with sub-flavor *reversed*).

```
if. > @                               ← vertical bare-reversed, name @
  cond                                ← receiver
  then                                ← method arg 1
  else                                ← method arg 2

if. cond then else                    ← horizontal form, receiver=cond, args=then, else

if. false 1:a 2:b > second            ← horizontal form, receiver=false (no binding per R-6.6.3),
                                      ← method args 1:a and 2:b (both bound)

joined. *1 > sixth                    ← compact-tuple-reversed, N=1
  text ""                             ← receiver (occupies the 1 direct slot)
  x                                   ← tuple element
  y                                   ← tuple element
```


Illegal:

```
name. a b                             ← horizontal
  c                                   ← rejected: vertical continuation under horizontal reversed

name. > @                             ← bare reversed waiting for receiver
  .other                              ← rejected: first deeper line starts with `.`
```

### 3.9 Compact tuple — `head *N`

R-3.9.1. Syntactic form: a *head expression* followed by `*` and an optional integer `N` (default `0`). The head expression on a compact-tuple line is one of:
  - a `head` (bare identifier, `*`, or literal — though `*` as head is unusual on a compact-tuple line; see §9.4.2);
  - an `hmethod` with 0 horizontal args (e.g., `foo.bar *`);
  - a `bare-reversed` (e.g., `name. *N`, with the additional constraint of R-3.9.4).

  These are the same forms the grammar's `compactTuple : (hmethod | applicable | reversed) SPACE STAR INT?` admits. Heads carrying horizontal args (`happlication`, `reversed-with-hargs`) cannot also carry `*N` — the compact-tuple marker comes after the head and before any args.
R-3.9.2. The head's vertical children are partitioned:
  - The first `N` children become direct positional arguments of the head.
  - The remaining children are collected into a single `Φ.tuple @star` argument appended at the end.
R-3.9.3. If the number of children is less than `N`, the line is reported as an error.
R-3.9.4. **Reversed-dispatch heads need `N ≥ 1`.** If the head is a reversed dispatch, the receiver must occupy at least one direct slot. Stated once in R-3.8.5; reproduced here only as a cross-reference. Specifically: `name. *` (no `N`, defaults to 0) and `name. *0` are rejected at the classifier — the receiver would have no place to live.

R-3.9.5. **For non-reversed heads, `N = 0` is legal.** A 3-line block

```
seq * > @
  true
  false
```

is a perfectly valid compact tuple: the head is `seq` (a `head`-kind expression), `N` defaults to 0, and the two vertical children become elements of the synthesised `Φ.tuple`. This is the canonical "tuple of N elements as one arg" form. The N=0 restriction in R-3.9.4 applies *only* when the head is `bare-reversed` — for `head` and `hmethod`-with-0-hargs heads, N=0 is the default and supported shape.

Outer kind: **`compact-tuple`** (open for vertical children; closed for `.method` continuation until the block ends).

```
sprintf * > first                     ← N=0, all children tupled
  x
  y

sprintf *1 > second                   ← N=1, first child direct, remainder tupled
  x
  y
  z

sprintf *2 > third                    ← N=2, first 2 direct, remainder tupled
  x
  y
  z
```

### 3.10 Name suffixes (after any head construct)

There are **four base forms** of name suffix (mutually exclusive on any one line) plus **two inline-phi composite forms** that incorporate a base form. A line may carry at most one suffix overall.

**Base forms:**

| Form | Meaning |
| --- | --- |
| `> name` | Explicit name. Optional trailing `!` for const. Optional ` /sig` to declare an atom. |
| `>>` | Auto-generated name (deterministic, derived from line and column). Optional `!`. Optional trailing `NAME` — a file-local handle (R-3.10.12). Atom signature forbidden. |
| `+> name` | Truthy test attribute. `name` must be a `NAME` token, not `PHI` (`@`) — see R-6.3.5. Legal only at indent level 1 of a top-level object (§6.3). |
| `-> name` | Throwing test attribute — expected to throw. Same rules as `+> name`; emits the `-` marker prefix instead of `+` (§9.4). |
| (none) | Allowed unless the object is a plain child of a formation (§6.2). |

**Inline-phi composite forms** (introduce an inline-phi formation as the line's outer kind):

| Form | Meaning |
| --- | --- |
| `> [params] > name` | Inline-phi formation with **explicit** name. |
| `> [params] >>` | Inline-phi formation with **auto-generated** name (the right-hand side `>>` replaces `> name`). |

R-3.10.1. Inline-phi mechanics: the LHS of the leftmost `> [` is the formation's body (assigned to `φ` of the inline-phi formation); the formation itself is named by the suffix on the right. The LHS head must fit on the same line. When the LHS carries no horizontal args its `φ` is open and accepts a deeper-indent vertical-argument block; when the LHS is a happlication the line is closed for deeper-indent children (§4.5).

Examples:

```
right x.neg > [x] > left                  ← explicit name
f.put (f.as-number.plus 1) > [s] >>       ← auto name
"x" > [a b] > pair                        ← multi-param
```

R-3.10.2. `>>` cannot carry `/sig` (auto-named atoms are nonsense).
R-3.10.3. `> name! /sig` is rejected (const + atom forbidden together).
R-3.10.4. Const-marking (`!`) follows the main suffix table at the top of §3.10: any **name suffix** (explicit `> name!` or auto-generated `>>!`) may carry `!`. In inline-phi composite forms, the `!` attaches to the **RHS** name (`> [params] > name!` and `> [params] >>!`), never to the LHS expression — the LHS has no name slot of its own (see R-3.10.5). The only constraint specific to auto-names is R-3.10.2 (`>> /sig` rejected). Const-marking an auto-generated name has limited practical use but is syntactically valid; user choice.
R-3.10.5. The LHS of inline-phi (the body, before `> [`) cannot carry its own `!`. `!` is a name-suffix modifier; the LHS has no name of its own — the line's only name lives on the RHS.
R-3.10.6. The LHS of inline-phi must be non-empty: at least one expression with outer kind `head`, `hmethod`, or `happlication` must precede the `> [`. A bare `> [x] > name` with no LHS is rejected. *Note on parentheses:* a paren group is not its own outer kind — it is a head-shape inside `head`/`hmethod`/`happlication` (see R-3.6 "head ... is one of: A paren group `(...)`"). The LHS may therefore appear paren-wrapped, but the kind classification still resolves to one of the three listed above. Two further LHS restrictions tighten the grammar deliberately:
  - The LHS may **not** be a `reversed-with-hargs` line (e.g., `if. a b c > [x] > foo` is rejected).
  - The LHS may **not** be a `bare-formation` (e.g., `[x] > [y] > name` is rejected). The grammar's `onlyphi` rule admits `hformation` as LHS, but horizontal formations are entirely removed from the new spec (R-3.6.5), so this case is rejected by the formation-removal rule as well.

  Only the four kinds enumerated above (head, hmethod, happlication, paren group) are permitted as inline-phi LHS.
R-3.10.7. **Exactly one `> [params]` suffix per line.** Chained inline-phi suffixes (`expr > [a] > [b] > name`) are rejected, even though the underlying grammar can express them. The construct is unused in practice and adds parsing complexity for no gain.
R-3.10.8. **Inline-phi suffix variants.** The right-hand suffix on an inline-phi line may take any of these forms (optionally with `!` const on the name): `> [params] > name` (explicit name), `> [params] >>` (auto-generated name), `> [params] +> name` (truthy test attribute), `> [params] -> name` (throwing test attribute), or bare `> [params]` (auto-named, equivalent to `> [params] >>`). All shapes are accepted by the parser; the test-attribute forms additionally inherit the depth constraint of R-6.3.3. **Compact test shorthand `++>` / `-->`.** A parameterless test attribute whose only binding is the `φ` decoratee also has a collapsed spelling that merges the empty `[]` param list with the `++>` (truthy) or `-->` (throwing) head shorthand (R-6.3.6): `lhs ++> name` is sugar for `lhs > [] +> name`, and `lhs --> name` is sugar for `lhs > [] -> name`. The `++>` / `-->` marker is recognised in this inline-phi suffix position only when preceded by a space (a leading `++>` / `-->` at the head of a line is the bare-formation shorthand of R-6.3.6, not an inline-phi suffix). XMIR emission is identical to the expanded `> [] +> name` / `> [] -> name` form.
R-3.10.9. Anything after the name is reported as "unexpected content" without aborting the line.
R-3.10.10a. **Anonymous inline-phi as a paren-grouped value.** Inline-phi formations are normally line-level suffixes (R-3.10.1), but the bare form `body > [params]` (no `> name` / `>>` / `+> name` on the right) is **also legal inside a paren group** as a value-level expression: `(body > [params])`. The group then evaluates to an anonymous formation with the given params and `body` bound to its `φ` slot; the enclosing context (a horizontal arg, the LHS of a binding, etc.) supplies any naming. Only the bare (anonymous) form is permitted in this position — `(body > [params] > name)`, `(body > [params] >>)`, and `(body > [params] +> name)` are rejected: naming and test attributes must attach at line level. The construct is rare; the canonical use is passing a one-parameter formation as a horizontal arg without dedicating a separate line, e.g. `malloc.of 8 (m.put 10 > [m])`.

**Atom signature `/sig`.** The `sig` is a **type variable**, or a single `NAME`, or a dotted path of names optionally rooted at `Q`:

```
/sig      ::=  type-var
            |  NAME ('.' NAME)*
            |  'Q' ('.' NAME)+
type-var  ::=  'A' | 'B' | 'C' | 'D' | 'E' | 'F'
```

R-3.10.10. `sig` declares the atom's return type. A `NAME`/dotted form names a **concrete** forma (`/number`, `/bytes`, `/Q.org.eolang.number`); a `type-var` declares a **generic** return — a universally-quantified type variable scoped to the atom. Same letter ⇒ same type throughout that atom; distinct atoms are independent. The variable set is capped at `A`–`F` (six) for now; any other letter, or a multi-character uppercase-initial token, used where a variable is expected is rejected (`type variable must be one of A-F`). A return signature carries **no** `?`: the optional marker is legal only on a void attribute (§3.4.8), so `/A?` on a return is rejected (`optional marker ? is allowed only on a void attribute`). A bare `/Q` (root alone, no dot-name) is rejected, as are the other malformed sigs (bare `/`, trailing dot `/Q.`, sigs starting with `.`).
R-3.10.11. The leading `Q` in a dotted concrete `sig` is promoted to `Φ` in XMIR (the source→XMIR mapping table in §9.3 is the single source of truth for all Q→Φ / @→φ / ^→ρ promotions). A `type-var` is emitted **verbatim** — never `Φ`-promoted, never alias-expanded, never homed by `add-default-package` (§9.3).

R-3.10.12. **File-local handles — `>> name`.** A `>>` auto-name suffix may carry an optional trailing `NAME`: a *file-local handle*. The object stays **anonymous** — it still receives its cactus `@name` (§9.2) and never enters the visible namespace — but `name` becomes a typeable alias for that cactus name, usable anywhere in the same `.eo` file (`resolve-local-names`, §9.2, rewrites references to the cactus name). So an anonymous helper can recurse by its handle or be reached from a sibling — unlike plain `> name`, which would expose `name` on the enclosing object's public surface. Accepted uniformly wherever bare `>>` is (bare formation, inline-phi formation, application, vertical void R-3.4.7); `!` const stays allowed (`>>! name`) except on a vertical void, `/sig` stays forbidden (R-3.10.2). A handle declared twice in one file is a compile-time error (`duplicate local name 'name'`); a reference with no matching handle is left untouched for later scope resolution. See §9.2 for the emission and the `handle → cactus-name` rewrite.

### 3.11 Triple-quoted text block — `"""`

R-3.11.1. An opening `"""` with nothing else on the line opens a block.
R-3.11.2. Body lines must be indented at least as deep as the opening; the block's text is the body lines joined by `\n` with the opening indent stripped.
R-3.11.3. A `"""` on its own line (at the opening indent) closes the block.
R-3.11.4. After the closing `"""`, a method chain is allowed (`""".as-bytes`), then the optional binding and suffix.
R-3.11.5. An unclosed block is reported at end-of-stream as a recoverable error.

```
"""
hello
world""" > text-value                 ← multi-line string named text-value
```

### 3.12 Inline binding — `:label` or `:N`

R-3.12.1. After an object body in an argument position, `:label` may appear, setting the attribute the argument attaches to.
R-3.12.2. `:N` (digit) maps to `α0`, `α1`, … positional slot names.
R-3.12.3. Binding is forbidden inside formation context — plain children carry their name via the name suffix, not the binding (§6.2).
R-3.12.4. Binding rules in same-indent groups (§6.6) apply.

### 3.13 Multi-line BYTES literal

A BYTES literal that ends a line with a trailing `-` continues on the next line. This is the one exception to "lines are atomic units of classification" — the lexer must recognise the continuation before line classification (§3.1) runs.

R-3.13.1. A BYTES token has one of three forms:
- `--` — empty bytes.
- `BB-` — a single byte (two hex digits) followed by `-`. **Always complete on its own line** — the multi-line continuation rule (R-3.13.3) applies only to chunks of two or more bytes, matching the grammar's `LINE_BYTES : BYTE (MINUS BYTE)+`. A bare `CA-` on a line by itself is therefore a single-byte literal, never the opening chunk of a multi-line BYTES.
- `BB-BB(-BB)*` — two or more bytes joined by `-`, optionally followed by `-` and a newline, then another `BB(-BB)*` chunk. Continuation may repeat. The trailing `-` signalling continuation requires a chunk of ≥2 bytes; a one-byte chunk cannot trigger continuation.

R-3.13.2. The continuation indent of the second and subsequent chunks must be at least as deep as the indent of *the line that began the BYTES token* (the first chunk's line, not the enclosing expression). Lower indent terminates the literal and is an error.

R-3.13.2a. **Position attribute for multi-line BYTES.** The emitted `<o>` for a multi-line BYTES literal records `@line` and `@pos` from the **first chunk's line** (the line where the token starts), not from the continuation line. The token spans multiple source lines but is positioned at its opening.

R-3.13.3. The continuation chunks are part of the **same token** — they do not produce separate LineShape records. The classifier sees one line containing the whole multi-line literal, with its source span covering all the affected source lines.

R-3.13.4. A comment, blank line, or any non-byte content inside the continuation is an error.

Example:

```
size.
  CA-FE-                               ← first chunk, ends with trailing `-`
  BE-BE                                ← continuation chunk
```

The two indented lines under `size.` form **one** BYTES literal `CA-FE-BE-BE`, occupying one argument slot of the reversed dispatch `size.`.

Illegal:

```
size.
  CA-FE-
                                       ← rejected: blank line inside multi-line BYTES
  BE-BE

size.
  CA-FE-
  # a comment                          ← rejected: comment inside multi-line BYTES
  BE-BE
```

**Implication for the classifier.** Before §3.1 runs, the lexer must scan ahead: if a line's last non-whitespace character is `-` and that line contains a partial BYTES token, the lexer consumes additional lines until the BYTES token is complete, then emits a single virtual line for classification. The indent stack (§5) is unaffected — the multi-line BYTES is one expression at one indent.

### 3.14 Pipe application — `| [arg…] [> name]`

A line whose first non-space character is `|` is a *pipe-application line*. It applies arguments to the **same-indent predecessor** — the object declared on the lines just above it — without naming that object at the call site. This is the surface form of phi-calculus *formation-with-application* `⟦…⟧(…)`: the predecessor is formed, then the pipe supplies its arguments. The `|` reads as an up-arrow to "the object above".

R-3.14.1. The `|` is followed by a single space, then either a horizontal argument list (§3.6) or nothing, then an optional name suffix (§3.10). Its tail is parsed exactly as an application's argument list plus suffix — the pipe supplies the arguments; the *head* is the implicit predecessor.

R-3.14.2. **Predecessor requirement.** The stack top at the pipe's indent must be a **formation** (`bare-formation` or `inline-phi-formation`) or another **pipe-application**, and it must be **named** (an explicit `> name` or an auto-generated `>>`). A pipe with no predecessor (top-level / empty stack), a deeper-indent ("descending") pipe, or a pipe whose predecessor is an unnamed formation, a plain value, an application, or any `.method` dispatch is an error. The named requirement is what lets the pipe refer to the predecessor by name (R-3.14.7); an unnamed formation cannot be a pipe target — give it a `>>`.

R-3.14.3. **Two forms**, distinguished by whether horizontal args are present on the line:
  - **Horizontal form** — `| arg1 arg2 … [> name]` (≥1 arg): the args are the application arguments. The line takes no deeper-indent children (`vertical-completed`), but may still be wrapped by a same-indent `.method` (§3.5) or extended by a following pipe (chained application, R-3.14.5).
  - **Vertical form** — `| [> name]` (0 args on the line): a deeper-indent argument block follows, exactly as under a `vapplication` head (§4.1). The args become the application arguments.

R-3.14.4. **No pipe after `.method`.** A pipe may follow a formation or another pipe only. Once a `.method` has dispatched an attribute off the predecessor, the formation is no longer the object in hand, so a pipe after a `.method` line (of any completion state) is rejected. Cross-line ownership: §5.2 (R-5.2.4a / R-5.2.5a / R-5.2.11a). Conversely a `.method` **after** a pipe is legal (the pipe application is a complete value); a pipe after a pipe is legal (R-3.14.5).

R-3.14.5. **Chaining.** Consecutive pipe lines build left-associated applications: `| a` then `| b` after formation `F` is `((F a) b)`, two applications. Each pipe in a chain is its own object and so must be named (R-3.14.2 applies to the *predecessor*, which for the second pipe is the first pipe). Contrast a single `| a b` (one application, two args).

R-3.14.6. Name suffix per §3.10: `> name`, `>>`, or none (the last only when the pipe is an unnamed intermediate immediately wrapped by a `.method`, which names the whole chain). The atom signature `/sig` and the test attribute `+> name` are rejected — a pipe is an application, not a formation. All-or-nothing inline binding (§6.6) applies to the argument group.

R-3.14.7. **Emission / XMIR.** A pipe line desugars to an ordinary application whose head is a reference to the (named) predecessor. So `| a > r` after a formation `F` (named `F`) is identical in XMIR to `F a > r`; `| a` then `| b` after `F >>` (auto-name `A`) is `A a` (auto-named) followed by `A′ b`. The parser emits the pipe line as a base-less `<o pipe=''>` with the args as children; the `wrap-applications` reshape (§9) sets `@base` from the preceding sibling's `@name` and drops `@pipe`, so every downstream pass (scope resolution, base rolling) treats it as a hand-written application.

R-3.14.8. **Predecessor placement — body vs argument block.** Where the predecessor formation ends up depends on where the pipe sits:
  - **Body of a formation** (the pipe's parent is abstract): the predecessor stays in place as a named attribute alongside the pipe application, so both are visible to siblings. `[x] > foo` then `| 5 > foo5` yields two attributes, `foo` and `foo5 = foo 5`.
  - **Argument block** (the pipe's parent is an application, so its siblings are collected as positional arguments): leaving the predecessor in place would make the enclosing application receive it *and* the pipe application as two arguments. Instead the predecessor **floats up** to the nearest enclosing abstract object (via `vars-float-up`, §9) and leaves no argument slot of its own, so the enclosing application receives exactly one argument — the pipe application referring to the floated definition. `wrap-applications` marks such a predecessor with a transient `@float-up`; `vars-float-up` consumes it. For example,

    ```
    [] > foo
      bar > @
        [] > hello
        | 5
    ```

    is identical in XMIR to

    ```
    [] > foo
      bar > @
        $.hello 5
      [] > hello
    ```

Outer kind: **`pipe-application`** (openness `open` for the vertical form's body, `vertical-completed` for the horizontal form).

```
[x] > foo                             ← named formation
  x.plus x > @
| 5 > foo5                            ← foo5 = foo applied to 5 (i.e. (5).plus 5)

[a b] >>                              ← auto-named (anonymous) formation
  a.plus b > @
| 2 3 > pair                          ← one application, two args, referring to the auto-name
```

Illegal:

```
6 > six
| 5 > n                               ← rejected: predecessor `six` is not a formation or pipe

[x] > foo
  ...
.x
| 6 > n                               ← rejected: pipe after a `.method`

| 5 > n                               ← rejected: no object above
```

### 3.15 Self-reference — `%`

A `%` token is *syntactic sugar for the auto-generated name of the anonymous formation that surrounds it* — the closest ancestor introduced by the `>>` suffix (§3.10). It is not a new reference mechanism: at compile time `%` is simply replaced by that name, exactly as if the (otherwise untypeable) auto-name had been written by hand. Its purpose is recursion without exposing a name — an anonymous helper (`fibo`, say) can call itself with `%` while staying inlined, invisible to the enclosing scope.

R-3.15.1. `%` is a value: it is recognised wherever a value is expected — as a line head (`% (n.minus 1)`), as a horizontal argument, and inside a paren group — and takes horizontal arguments (§3.6) and a `.method` chain (§3.5) with the ordinary application shape.

R-3.15.2. **Scope.** `%` stands for the **nearest enclosing anonymous formation** — the closest ancestor whose auto-generated name carries the cactus prefix (§9.2). A named formation (`> name`) is not a target; reference such an object by its name. A `%` with no enclosing anonymous formation is a compile-time error.

R-3.15.3. **Emission / XMIR.** The parser emits a base-less `<o self=''>` marker; the `resolve-self` reshape (§9) substitutes the name — it sets `@base` to the enclosing anonymous formation's `@name` and drops `@self` — so every downstream pass sees a plain reference by the auto-name. A `%` outside any anonymous formation is rejected there with a `resolve-self` check error (a post-parse check, like the compact-tuple index check, not a §9.9 parser error).

```
io.stdout > @                         ← prints the 5th Fibonacci number
  tt.sprintf *1
    "The 5th Fibonacci number is %d\n"
    [n] >>                            ← anonymous (auto-named) formation a🌵…
      if. > @
        n.lt 2
        n
        plus.
          % (n.minus 1)               ← % = the anonymous formation's auto-name
          % (n.minus 2)               ← % = the anonymous formation's auto-name
    | 5                               ← applies the formation to 5 (§3.14)

[] > app
  % 6 > @                             ← rejected: no enclosing anonymous formation
```

Outer kind: that of the underlying application (§3.6), since `%` is just a head value.

---

## 4. Multi-line expressions and their outer kinds

Single-line expressions (§3) carry their full meaning on one line. Multi-line expressions span a head line plus deeper children, with their outer kind determined by what follows.

### 4.1 Vertical application — `head` + child block

A line whose outer (after classification) is `head` or `hmethod` (0 args) becomes a **`vapplication`** as soon as at least one deeper-indent child line appears.

```
malloc.for > @                        ← head line: hmethod with 0 args + name
  0                                   ← first vertical arg
  [m] >>                              ← second vertical arg
    body
```

The naming line is the **head line** (the first line).

Outer kind: **`vapplication`**.

### 4.2 Vertical method chain — `head` (or vapp head) + same-indent `.method` continuations

A line whose outer was `head`, `hmethod` (0 args), `bare-formation`, or any kind ending its block in `vertical-completed` state, may be followed by a same-indent line starting with `.method`. The whole chain forms a **`vmethod`**.

```
tmpdir                                ← head line
.tmpfile                              ← continuation 1
.deleted                              ← continuation 2
.open > @                             ← continuation 3, naming line
  "r"                                 ← vapp arg of .open
  f > [f]                             ← vapp arg of .open
```

The naming line is the **last `.method` line** of the chain. If the last `.method` also has vapp args below it, the chain's outer kind becomes **`vapplication` over a `vmethod` head**, and the naming line is still that last `.method` line (which is also the vapp head).

Outer kind: **`vmethod`** (or `vapplication` if vapp args are attached after).

### 4.3 Reversed dispatch — vertical form

See §3.8.3. The receiver and method args live as deeper-indent children. After the child block ends, the expression's outer kind remains **`bare-reversed`** but its openness transitions to `vertical-completed` (it may be wrapped by a same-indent `.method` per §6.1).

### 4.4 Compact tuple

See §3.9. After its child block, the kind remains **`compact-tuple`** but its openness transitions to `vertical-completed`.

### 4.5 Inline-phi formation

A line whose suffix is `> [params] > name` is an **`inline-phi-formation`**: the LHS of `> [` is the formation's `φ`, and the bracket params are its voids. Whether the line accepts a deeper-indent body depends on the LHS:

- **Bare φ (LHS has no horizontal args)** — the φ is `open`. An **unnamed** deeper-indent line attaches to it as a **vertical application argument**, exactly as it would under a plain `vapplication` head. So `foo > [x] > bar` with an unnamed body block is `[x] > bar` whose `φ` is `foo` applied to that block:

  ```
  foo > [x] > bar                     ← φ = foo, param = x, name = bar
    x.plus 42                         ← first vertical arg of foo
  ```

  is identical in XMIR to:

  ```
  [x] > bar
    foo > @
      x.plus 42
  ```

- **φ with horizontal args (LHS is a happlication)** — the φ is already a complete application, so the line is **`horizontal-completed`** and accepts no body (R-6.1.1).

- The line may freely appear as a vertical arg of an enclosing expression — the rules above apply only to *its own* depth.

An only-phi formation may carry **named attributes besides its `φ` decoratee**. A deeper-indent body line is classified by whether it is named:

- **Unnamed** — a vertical application argument of `φ` (the decoratee), as above. The φ stays `open` and absorbs it.
- **Named** (`> name`, `>>`, `+> name`) — a **sibling attribute of the formation**, alongside `φ`. The first named body line closes the φ; it and every later body line are the formation's own attributes, laid out exactly like the body of a plain formation. Unnamed vertical arguments of `φ`, when present, must therefore precede the named attributes.

So `bar > [] > foo` with a named body line is the formation `[] > foo` whose `φ` is `bar` and which also binds `b`:

```
bar > [] > foo                        ← φ = bar, name = foo
  a1 > b                              ← sibling attribute b of foo (not an argument of bar)
    42
```

is identical in XMIR to:

```
[] > foo
  bar > @
  a1 > b
    42
```

The classification applies only to a **direct** body line of the only-phi formation. A name suffix deeper inside a `φ` argument is an ordinary named vertical argument of that nested application and is accepted as usual (§6.2, contrast C.3):

```
foo > [] >>
  bar                                 ← unnamed φ argument (foo applied to bar)
    baz > x                           ← named vertical argument of bar, accepted
```

A nested formation argument still names its own attributes:

```
foo > [] >>
  [y]                                 ← unnamed formation argument
    z > w                             ← attribute of [y], named as usual
```

### 4.6 Triple-quoted text block

After the closing `"""`, the line's outer becomes **`text-block`** (single-line semantics, even though body spans many source lines).

---

## 5. The indent stack (cross-line FSM)

The parser maintains a stack of **level records**, one per occupied indent level. The stack is the single source of truth for cross-line rules (§6).

### 5.1 Level record

```
Level {
  indent: int
  kind: OuterKind             // updates as more lines arrive at deeper indents
  openness: { open, vertical-completed, horizontal-completed }
  parent_kind: OuterKind | top-level
  parent_is_atom?: bool       // true if the entry one indent shallower is a formation with /sig suffix
  is_atom?: bool              // true if this entry itself is a formation with /sig suffix
  named?: bool                // true once a name suffix has been seen on the naming line
  start_line: int             // for error messages
  // kind-specific:
  receiver_consumed?: bool    // for bare-reversed
  compact_tuple_n: int        // for compact-tuple
  child_count: int            // for compact-tuple's count check
  // for cross-sibling validation:
  args_binding_state: { unknown, all-bound, all-unbound, mixed } // tracks the all-or-nothing
                              // binding rule (R-6.6.2) across vertical args of this parent
}
```

R-5.1.1. The stack's `indent` values are strictly increasing from bottom to top.
R-5.1.2. The stack always has indent step exactly 2 between adjacent entries.
R-5.1.3. **Atoms.** A formation with `/sig` is structurally a `bare-formation` kind plus the `is_atom?` flag set true. Rules that say `parent_kind = atom` (e.g., R-5.3.4) read `parent_is_atom?` on the new entry, which equals `is_atom?` of the entry below.

### 5.1.1 Global parser state

Beyond the indent stack, the parser maintains a small set of global flags. These are scalars, not per-level, and survive across pops:

```
ParserState {
  first_object_emitted?: bool          // flips true when any non-meta object is parsed; used by R-3.2.2
  pending_blank_count: int             // blank lines seen since the last non-blank line; consumed by R-6.5.3 timing
  trailing_blank_count: int            // blank lines since EOF; checked by R-6.5.6 / R-8.4
  in_text_block?: bool                 // currently inside an open triple-quoted block (§3.11)
  text_block_open_line: int            // for unclosed-text-block error
  pending_comment_block: List<Line>    // top comment lines buffered until the first meta/object flushes them (§6.4)
}
```

### 5.2 Per-line transitions

When a non-blank, non-comment line at indent `N` arrives (after the classifier in §3 emits its LineShape record):

**Step A — Pop deeper levels.**

R-5.2.1. For each stack entry with indent > `N`, pop it. Run its close-time checks (§5.3).
R-5.2.2. When popping an entry at indent `K + 2` (so the new top is at indent `K`): the top's `openness` transitions from `open` to `vertical-completed` (its block has finished, but a same-indent `.method` may still wrap it). If the top is already `horizontal-completed`, leave it unchanged.

**Step B — Same-indent line.** If the top entry now has indent = `N`:

R-5.2.3. **MethodDispatch line dispatch.** If the line's kind is `MethodDispatch` (`.method` continuation):
  - **(a) Extend:** If the top's openness ∈ {`open`, `vertical-completed`} AND the top's kind is **not** in the horizontally-completed set (Appendix A) AND the top's kind is **not** `inline-phi-formation`: extend the top's kind to `vmethod` (or to `vmethod-with-hargs` if the new line carries ≥1 hargs), update `named?` if the line carries a name suffix, leave openness `open` (or transition to `horizontal-completed` if the new kind is `vmethod-with-hargs`).
  - **(b) Reject — horizontally-completed predecessor:** If the top's openness is `horizontal-completed` (equivalently, kind ∈ horizontally-completed set): error `method continuation not allowed after horizontal application` (§9.9).
  - **(b′) Reject — only-phi predecessor:** If the top's kind is `inline-phi-formation` (whose bare-φ form is `open` but is not a method-chain host): error `method continuation not allowed after only-phi formation` (§9.9).

R-5.2.4. **Non-MethodDispatch same-indent line.** If the line's kind is **not** MethodDispatch: the top entry is a *completed previous sibling*. Run close-time checks (§5.3) on it, then **replace** it with a new entry built from the new line. The new entry's `parent_kind` is read from the stack entry below.

R-5.2.4a. **PipeApplication same-indent line.** A `PipeApplication` line (§3.14) is a Non-MethodDispatch line and so replaces the top per R-5.2.4, but first the top must satisfy R-3.14.2: its kind must be `bare-formation`, `inline-phi-formation`, or `pipe-application`, and it must be named. Otherwise error `a pipe must follow a named formation or another pipe` (§9.9). In particular a top whose kind is `vmethod` / `vmethod-with-hargs` (a `.method` was taken off the predecessor) fails this check — R-3.14.4.

**Step C — Deeper line.** If the top entry now has indent < `N`:

R-5.2.5. If the line's kind is `MethodDispatch`: error `method continuation has no expression to attach to`.
R-5.2.5a. If the line's kind is `PipeApplication`: error `a pipe must follow a named formation or another pipe` — a deeper-indent ("descending") pipe has no same-indent predecessor to apply to. A `.method` line at indent `N` requires a previous sibling expression at the same indent; a deeper-than-parent position has no such sibling. **This rule is the authoritative owner of the `.method`-as-deeper-line rejection**, including the bare-reversed-receiver edge case (a `.method` line as the first deeper child of a bare-reversed parent). R-5.2.9's "must not start with `.`" condition is enforced *via this rule*; R-5.2.9 itself only manages the `receiver_consumed?` flag.
R-5.2.6. The previous top's openness must be `open`. If `vertical-completed` or `horizontal-completed`: error `unexpected deeper-indent line — previous expression is closed for children`.
R-5.2.7. `N` must equal `previous_top.indent + 2`. Otherwise: error `indent increased by more than one level`.
R-5.2.8. Push a new entry. Its `parent_kind` is the previous top's `kind`.
R-5.2.9. If `parent_kind = bare-reversed` and the previous top's `receiver_consumed?` is false: this deeper line is the receiver. (The line-starts-with-`.` rejection has already fired in R-5.2.5 if applicable; this rule only manages the `receiver_consumed?` flag.) Mark `receiver_consumed? = true` on the previous top.

**Step D — Top-level line.** If the stack is empty (no entry at indent ≤ `N`):

R-5.2.10. If the line's kind is `MethodDispatch`: error `method continuation has no expression to attach to` (§9.9). A `.method` line requires a previous-sibling expression at the same indent; an empty stack has no such sibling.
R-5.2.11. Otherwise: the line is the program's top-level object. Push a new entry with `parent_kind = top-level`. If the line's kind is `Meta` and a non-meta object has already been emitted: error (R-3.2.2).
R-5.2.11a. If the line's kind is `PipeApplication`: error `a pipe must follow a named formation or another pipe` — an empty stack has no object above to apply to.

### 5.3 Close-time checks

When a level record is popped or replaced:

R-5.3.1. **Naming check.** If `parent_kind = formation` or `parent_kind = top-level`, then `named?` must be true. Otherwise: error "object inside formation must have a name" at `start_line`.
R-5.3.2. **Bare reversed completeness.** If `kind = bare-reversed` and `receiver_consumed? = false`: error "reversed dispatch missing receiver."
R-5.3.3. **Compact tuple count.** If `kind = compact-tuple` and `child_count < compact_tuple_n`: error "compact tuple `*N` requires at least N children, got `child_count`."
R-5.3.4. **Atom body.** If the popped entry's `parent_is_atom?` is true, the popped entry's kind must be `bare-formation` **AND** its name-suffix form must be a test attribute (`+>` or `->`) (R-6.3.1). Otherwise: error `atom may contain only test attributes`. (`+>` is a name-suffix variant, not a property of the kind itself; this rule checks both fields.) Note: even when this check passes, the `+>` child must additionally satisfy the depth constraint of R-6.3.3 (verified separately by R-5.3.5); a `+>` child of a *nested* atom fails R-5.3.5 because tests are legal only at indent 2 of a top-level object.

R-5.3.4a. **R-5.3.4 and R-5.3.5 are disjoint.** Both rules check the popped entry's suffix and parent, but on disjoint conditions: R-5.3.4 fires *only when* the popped entry is **not** a `+>` test (so it can't have `+>` to feed R-5.3.5); R-5.3.5 fires *only when* the popped entry **is** a `+>` test (so the atom-body check passes vacuously). A single popped entry cannot trigger both rules. Multiple errors per source line are possible only when the line introduces *multiple* level records that each independently fail (e.g., a nested atom *and* a deeply-placed test in the same file), but in that case each error attaches to its own entry's `start_line`.
R-5.3.5. **Test depth.** If the popped entry's name suffix is `+>`, the popped entry's *immediate parent* must itself be a top-level formation (sitting at indent 0). Concretely:
  - the popped entry's `parent_kind` must **not** be `top-level` (otherwise the popped entry is itself at indent 0, which is illegal), AND
  - the popped entry's parent's own `parent_kind` must be `top-level` (otherwise the parent is itself nested, also illegal).

  Either failure yields error `test attribute legal only as direct child of top-level object` (R-6.3.3 / §9.9). The first failure case covers a stray `+>` at indent 0; the second covers `+>` at indent ≥ 4.
R-5.3.6. **Inline-phi body rules.** An `inline-phi-formation` whose φ carries horizontal args is closed for deeper children (caught at child-push time via R-6.1.1). When its φ is bare, each direct deeper-indent body line is classified by whether it is named (§4.5): an **unnamed** line attaches to φ as a vertical argument (`@as='αN'`); a **named** line closes φ and emits as a sibling attribute of the formation (`<o>` of the formation, alongside `φ`). A name suffix deeper inside a φ argument is an ordinary named vertical argument of that nested application and is unaffected. There is therefore no naming ban on an only-phi body — the former `argument of an only-phi formation cannot carry a name suffix` error is retired.

### 5.4 End-of-file (FSM action)

R-5.4.1. At EOF the FSM pops all remaining stack entries, running close-time checks (§5.3) for each. The semantics, error handling, and additional EOF-only validations (unclosed text block, top comment block flush, trailing blank lines, span-checks across siblings) are specified in **§8 — End-of-stream validation**. This subsection exists only to mark the FSM action; §8 is the authoritative validation pass.

---

## 6. Cross-line rules (derived from §5)

These rules state the contract in terms of cross-line behavior. Each rule is mechanically realised by the stack operations in §5; this section gives the language-level reading.

### 6.1 Notation consistency

A horizontally-completed expression cannot be extended. Specifically:

R-6.1.1. An expression whose outer kind is in the horizontally-completed set (defined once in Appendix A — currently `{happlication, reversed-with-hargs, vmethod-with-hargs}`) admits:
- No deeper-indent children.
- No same-indent `.method` continuation.

R-6.1.2. An expression whose outer kind is one of `head`, `hmethod` (0 hargs), `bare-formation`, `bare-reversed`, `compact-tuple`, `vapplication` (in progress), or `vmethod` (in progress, with 0 hargs on the most recent link) is **open**. The first five are the kinds a line classifies as on its own; `vapplication` and `vmethod` are the *transitional* kinds the FSM (§5.2.3) reaches after extending a `head` / `hmethod` with deeper-indent args or same-indent `.method` continuations. Either way, the openness rule is:
- May receive deeper-indent children (each kind has its own role assignment for those children).
- May not be wrapped by a same-indent `.method` until the child block has closed (transitioning openness from `open` to `vertical-completed`).

R-6.1.3. An expression whose openness is `vertical-completed` — i.e., `vapplication`, `vmethod`, `bare-reversed`, `bare-formation`, or `compact-tuple` whose child block has ended — is:
- May not receive more deeper-indent children (the block is over).
- May be wrapped by a same-indent `.method`, which transitions it back to `vmethod` (still `open`).

R-6.1.4. An `inline-phi-formation` is governed by its openness rather than a fixed set membership. A φ with horizontal args starts `horizontal-completed` (R-6.1.1). A bare φ is `open` and admits deeper-indent children — its φ's vertical arguments (§4.5) — but, unlike the open kinds of R-6.1.2, it may **not** be wrapped by a same-indent `.method` (error `method continuation not allowed after only-phi formation`, §5.2.3(b′)); wrapping the formation would misattribute the method as a second attribute.

Examples:

```
foo                                   ← head, open
  a                                   ← deeper arg: now foo is vapplication-in-progress
  b
.method > result                      ← same-indent .method after block closed: vmethod, naming-line

foo a b                               ← happlication, horizontally-completed
.method > result                      ← rejected: cannot wrap horizontal app

io.stdout                             ← hmethod (0 hargs), open
  string.sprintf                      ← deeper arg, fine

x.put 2                               ← happlication, horizontally-completed
  something                           ← rejected: closed for deeper children
```

### 6.2 Naming inside formations

R-6.2.1. Every expression that is a plain child of a formation **must carry a name suffix** on its naming line.
R-6.2.2. The naming line per outer kind:

| Outer kind | Naming line |
| --- | --- |
| Single-line expression (`head`, `hmethod`, `happlication`, `reversed-with-hargs`, `inline-phi-formation`) | that line |
| `vapplication` | the head line (first) |
| `vmethod` (chain of same-indent `.method` continuations) | the **last** `.method` line |
| `vapplication` whose head is `vmethod` (e.g., `.open > @ / "r" / f`) | the last `.method` line (also the vapp head) |
| `bare-formation` (block follows) | the `[...]` line |
| `bare-reversed` (vertical form) | the `name.` line |
| `compact-tuple` | the head line carrying `*N` |
| `text-block` | the closing-`"""` line |

R-6.2.3. Intermediate lines within a multi-line expression **may** carry their own optional names (binding intermediate sub-expressions); these are independent of the outermost name and are not required.

Examples:

```
[] > foo
  x.y.z > name                        ← single-line: name on its only line

  tmpdir                              ← intermediate, no name (optional)
  .tmpfile > file                     ← intermediate, named (optional)
  .deleted                            ← intermediate, no name
  .open > @                           ← naming line: outer vmethod is named @

  malloc.for > result                 ← vapplication head, naming line
    0
    [m] >>
```

Illegal:

```
[] > foo
  x.y.z                               ← rejected: plain child has no name

  tmpdir                              ← outer vmethod starts here
  .tmpfile
  .open                               ← rejected: vmethod's naming line carries no suffix
```

### 6.3 Atoms and test attributes

R-6.3.1. A formation declared with `/sig` suffix on its name is an **atom**. An atom may contain only test attributes (truthy `+>` or throwing `->`); regular bound or master children are rejected.
R-6.3.2. A non-atom formation may contain any mix of plain children, master children, and test attributes.
R-6.3.3. A test attribute (`+>` truthy or `->` throwing) is legal **only** at indent level 1 (indent 2 spaces) — i.e., as a direct child of the top-level object. Test attributes at any other depth are rejected.
R-6.3.4. Atoms may appear at any nesting depth, with two restrictions:
  - **(a)** A nested atom (one not at indent 0) cannot hold tests (R-6.3.3 — `+>` legal only at indent 2 of top-level) and cannot hold regular children (R-6.3.1 — atoms accept only test children). Therefore a nested atom's body must be **empty**.
  - **(b)** A nested atom is legal only when the containing formation is **not itself an atom**. Atoms inside atoms are rejected: an atom's body may contain only `+>` test attributes (R-6.3.1), and a master child (formation/atom) of an atom is therefore inadmissible regardless of body shape.
R-6.3.5. A test attribute name (truthy `+>` or throwing `->`) must be a `NAME` token. `+> @` / `-> @` (PHI as test name) is rejected even though the underlying grammar's `tname : tarrow (PHI | NAME)` accepts it. Tests are named identifiers; `@` has no meaning as a test name.
R-6.3.6. **Test-attribute shorthand.** A line whose first non-space characters are `++>` (truthy) or `-->` (throwing) is sugar for a bare parameterless formation with a test suffix: `++> name` ≡ `[] +> name`, `--> name` ≡ `[] -> name`. The two forms are equivalent in every respect after classification — same XMIR emission (§9.4), same depth constraint (R-6.3.3), same name rules (R-6.3.5). There is no ambiguity with meta directives: metas are legal only before the first object (R-3.2.2), start with `+`, and their names never begin with `+>`; a `-`-headed line is never a meta. The same `++>` / `-->` markers are also accepted in the **inline-phi suffix position** (`lhs ++> name` ≡ `lhs > [] +> name`, `lhs --> name` ≡ `lhs > [] -> name`, R-3.10.8), where a space precedes them; there they bind the LHS to the test attribute's sole `φ` decoratee. The throwing shorthand and its expanded `[] -> name` form select `Assertions.assertThrows` at transpile time purely from the `-` marker (§9.4); the truthy forms select `Assertions.assertTrue`.

Examples:

```
[] > foo                              ← top-level formation
  bar > attr1                         ← plain child (regular)
  [] > inner-formation                ← master child (regular)
  [] +> test-foo                      ← test attribute, legal at indent 2

[] > foo /number                      ← top-level atom
  [] +> test-foo                      ← only test attributes allowed inside

[] > foo
  [] > bar
    [] > baz /number                  ← nested atom at indent 4; legal but body must be empty
```

Illegal:

```
[] > foo /number
  bar > attr                          ← rejected: atom can't have regular children

[] > foo
  [] > inner
    [] +> test-inner                  ← rejected: tests must be at indent 2 of top-level
```

### 6.4 Comment attachment

R-6.4.1. The top comment block documents the whole program. It is flushed into `/object/comments` by the first meta or object (§9.5) and, in a comment-only file, at end-of-stream.
R-6.4.2. A comment that is indented, follows a meta, follows an object, or opens a second block is rejected: "comment is allowed only on top of the file, before metas."

```
# Documents the whole program.

+package foo

[] > foo                                  ← the top block above documents the program
  [] > bar
  # rejected — comments are allowed only on top of the file
  [] > baz
```

### 6.5 Blank-line policy

R-6.5.1. Inside the top comment block: blank lines forbidden.
R-6.5.2. After the top comment block: exactly one blank line separates it from the first meta or object. A block immediately followed by a meta or object, with no blank in between, is rejected: "a blank line must separate the top comment block from the rest of the file".
R-6.5.3. Before a master object (formation, atom, inline-phi formation): zero or one blank line. Before a `+>` test attribute: exactly one blank line; a test attribute sitting directly under the previous non-blank line is rejected: error "missing blank line before a `+>` test attribute (R-6.5.3); exactly one blank line must precede every test attribute".

*Note on validation timing and indent attribution.* A blank line carries no indent or kind of its own. The legality of a blank line is determined by the *next* non-blank line: when that line is classified, the parser checks whether it is a master. If yes, the preceding blank is legal; if no (the next line is a plain child or a non-master continuation), the blank is reported as `blank line not allowed between non-master siblings` (§9.9), with the position of the offending blank line. The blank logically attaches to the indent of the *next non-blank line*, not to whatever was popped during Step A (§5.2.1) when that next line arrives — Step A pops change the stack but not the blank's identity. `pending_blank_count` (§5.1.1) is read at the point the next non-blank line classifies.
R-6.5.4. Between two plain siblings: blank lines forbidden.
R-6.5.5. After the meta header: exactly one blank line separates metas from whatever follows.
R-6.5.6. At end-of-file: zero or one trailing blank line; more than one is an error.

Examples:

```
# Top-of-file comment.
                                          ← exactly one blank after the comment block
+architect yegor256@gmail.com
+version 0.0.0
                                          ← exactly one blank after metas
[] > foo
  a > attr1
  b > attr2                               ← no blank between bounds
                                          ← blank legal before next master
  [] > inner
    body > x
```

Illegal:

```
[] > foo
  a > attr1
                                          ← rejected: blank between plain siblings
  b > attr2
```

### 6.6 Bindings

R-6.6.1. A binding `:name` (or `:N`) names the attribute slot the argument attaches to. Without a binding, args attach by positional index.
R-6.6.2. **All-or-nothing rule.** Within a same-indent group of application arguments, bindings must be all bound or all unbound. Mixing is rejected.
R-6.6.3. **Receiver exclusion.** The receiver of a reversed dispatch is the dispatch target, not an argument, and cannot carry a binding. The remaining args of the dispatch form a normal application-arg group governed by R-6.6.2.
R-6.6.4. Inline binding on a method dispatch is legal only when that method is the **last** in its chain.

Examples:

```
if. true x y                              ← legal: all unbound
if. true x:a y:b                          ← legal: receiver unbound, args all bound
foo a:1 b:2                               ← legal: all bound
foo a b                                   ← legal: all unbound
```

Illegal:

```
foo a:1 b                                 ← rejected: mixed binding
if. true:a x y                            ← rejected: receiver carries binding
if. true x:a y                            ← rejected: args mixed
```

---

## 7. Error recovery

R-7.1. A parsing error throws at the cursor's failing position; the loop catches it and appends to `/object/errors/error` (§9.6).
R-7.2. The XMIR cursor is rolled back to a savepoint taken at the start of each line, so any dangling open elements from the half-parsed line are balanced.
R-7.3. **Stack recovery semantics.** Transitions that **completed** before the error stand — including any close-time checks already executed. Only the failing line's *half-built* additions are discarded: the line's XMIR (rolled back to the savepoint) and any partially-pushed level record. The stack as committed by prior lines is unchanged.

R-7.3.1. **Step A pops are not rolled back.** If Step A (§5.2 — popping deeper levels and running their close-time checks) ran successfully at the start of the failing line, those pops stay popped. Step A only acts on indent transitions, which are determined by the line's leading whitespace alone; that determination cannot fail. Any close-time check errors raised during Step A become separate `<error>` entries; the failing line's own error is independent.

R-7.3.2. The half-built additions discarded by R-7.3 are specifically: any extension to the top entry (R-5.2.3), any sibling replacement (R-5.2.4), any new push (R-5.2.7, R-5.2.11), and any updates to global parser state (§5.1.1).
R-7.4. The next non-blank line is processed against the post-recovery stack as if the failing line had not existed. Step A (§5.2) runs normally on it.
R-7.5. The parser continues processing subsequent lines.

**Example.** Source:

```
[] > foo
  bar > a
  ???                                ← rejected: unrecognised line shape
  baz > b
```

When line 3 fails, line 2's transitions (push, close `bar > a`'s slot, etc.) have already committed. Only line 3's partial work is discarded. Line 4 `baz > b` is then processed as a normal new sibling of `foo` — no cascade error, no double-reporting of `bar > a`.

---

## 8. End-of-stream validation

After the main loop completes:

R-8.1. Pop all stack entries and run close-time checks (§5.3). **If any close-time check fails**, emit an `<error>` element per R-9.6 and continue popping the remaining entries — one failure does not abort the EOF sweep. This is where `reversed dispatch missing receiver` (R-5.3.2), `object inside formation must have a name` (R-5.3.1), `atom may contain only test attributes` (R-5.3.4), and `compact tuple requires at least N children` (R-5.3.3) typically surface for the outermost open expressions.
R-8.2. Report any unclosed text block.
R-8.3. Flush a top comment block still pending in a comment-only file into `/object/comments` (§9.5).
R-8.4. Report excessive trailing blank lines (>1).
R-8.5. Run any all-or-nothing binding checks that span the popped entries.
R-8.6. After all popping and end-of-stream checks complete, the parser emits the closing tags for the `<object>` document. Errors emitted during EOF do not prevent emission of valid XMIR for the rest of the file.

---

## 9. XMIR emission

This section pins the concrete output decisions a parser must make so two conforming implementations produce the same XMIR for the same source.

### 9.0 Emission conventions

R-9.0.1. **There is no separate wrapper element.** `<o>` is the only element kind used for objects (plus auxiliary `<comment>`, `<comments>`, `<error>`, `<errors>` containers per §9.5–9.6 and `<part>` per §3.2). An expression's outermost `<o>` *is* the expression's container — its arguments and sub-expressions appear as direct children of that same `<o>`, not under a wrapper. (Two synthesised `<o>`s are injected as body children, not as wrappers around an expression: the `<o name='λ' atom='<sig>'/>` marker for atoms (§9.4) and the `<o base='Φ.tuple' star=''>` wrapper for compact tuples (§9.4 / R-3.9.2). Neither contradicts this rule.)

R-9.0.2. **Head-with-args constructs** (`happlication`, `vapplication`, `bare-reversed`, `reversed-with-hargs`, `compact-tuple`) emit a single `<o>` whose `@base` is the head's reference (or, when the head is itself a method-dispatch chain, the `<o>` of the *last link* of that chain — see R-9.0.3 below). Arguments appear as direct children of that `<o>` in source order. The head is NOT an additional sibling under some outer wrapper — it IS the outer `<o>` (modulo the chain-head case).

R-9.0.3. **Method-dispatch chains** (`hmethod` and `vmethod`/`vmethod-with-hargs`) are emitted **flat**: each chain link is a *separate* `<o>` carrying `@method=""` (empty value) and `@base='.<methodname>'`. The receiver of the chain emits as a preceding sibling under the same parent; the method links follow as siblings, in source order. The chain's outermost user-given name attaches to the **last** link's `<o>`. For **vmethod** chains specifically, R-6.2.3 also allows optional intermediate names: when an intermediate `.method` line carries its own `> name` suffix, that name attaches to the corresponding link's `<o>`. For **hmethod** (single-line) chains, intermediate names are not syntactically available — only the trailing link of the line can carry the line's name suffix. The flat-siblings form is the parser's output; the downstream XSL pass `wrap-method-calls.xsl` folds these flat siblings into a nested form for canonical XMIR. *That folding is outside the parser's contract* — this spec describes only the parser's flat output. R-9.4.1 reiterates that `@method` is the intermediate marker carrying this contract.

R-9.0.3.1. **Chained head with horizontal args.** When an `happlication` (or `vapplication`, etc.) has a method-dispatch chain as its head (e.g., `foo.bar 42 "x"`), the head's chain links expand per R-9.0.3 as flat siblings. The horizontal args of the happlication become **children of the chain's last link `<o>`**, in source order — *not* further flat siblings. This matches the `vmethod-with-hargs` emission shape (R-9.4.2), so the same logical construct (chain + hargs) has one canonical pre-fold form regardless of whether the chain is horizontal or vertical. The user-given name of the whole happlication attaches to the chain's last link `<o>`.

R-9.0.3.2. **Top-level hmethod expression.** A top-level `hmethod` expression (e.g., `foo.bar > x` as the entire program — `foo` is the receiver, `.bar` is the method link) emits multiple top-level sibling `<o>`s under the program root, one per chain segment, per R-9.0.3. The source-level "one expression" maps to multiple flat XMIR elements until `wrap-method-calls.xsl` folds them. (A line *starting* with `.method` at top level is forbidden by R-5.2.10; this rule covers only chains whose lead segment is an identifier.)

**Naming-check note for top-level chains.** R-5.3.1 requires top-level objects (parent_kind = top-level) to be named. The naming check operates on the **level record** (one per source line, §5.1), not on every emitted `<o>`. A top-level hmethod line has *one* level record with `named?=true` if the line carries `> name` (or `>>`). The fact that emission produces multiple flat sibling `<o>`s — only the last of which carries the `@name` attribute — does not multiply the naming requirement. R-5.3.1 is satisfied by the single level record's `named?` flag.

Example (`bar > x` containing `baz.m > y`):

```xml
<o name="x" base="bar" line="2" pos="2">
  <o base="baz" line="3" pos="4"/>                     <- receiver, flat sibling
  <o name="y" base=".m" method="" line="3" pos="7"/>   <- method link, flat sibling, carries the name
</o>
```

The two `<o>`s under `bar > x` are the parser's intermediate emission for the single hmethod argument `baz.m`. After `wrap-method-calls.xsl`, the same source becomes `<o name="x" base="bar"><o name="y" base=".m"><o base="baz"/></o></o>` — but that is post-parse XMIR, not what the spec demands the parser emit.

### 9.1 Position model

R-9.1.1. **Line numbers are 1-indexed.** The first source line is `line=1`.
R-9.1.2. **Column positions (`pos`) are 0-indexed.** The leftmost character of a line is `pos=0`. The `pos` attribute records the column where the first character of the object's source text begins.
R-9.1.3. For a `.method` line continuation, the position recorded is the column of the leading `.` (not the method name that follows).
R-9.1.4. For a multi-line expression, the position recorded on the top-level `<o>` is from the **head line** (the line where the expression starts), not the naming line if those differ.

Example:

```
[] > foo                       (line 1)
  bar > x                      (line 2, indent 2)
    baz.m > y                  (line 3, indent 4)
```

Emits:

```xml
<o name="foo" line="1" pos="0">
  <o name="x" base="bar" line="2" pos="2">
    <o base="baz" line="3" pos="4"/>
    <o name="y" base=".m" method="" line="3" pos="7"/>
  </o>
</o>
```

(`pos=7` for `.m` is the column of the dot: indent 4 + 3-char `baz` = column 7.)

### 9.2 Auto-name format

R-9.2.1. For an object whose name suffix is `>>`, the emitted `@name` is computed as:

```
auto_name(line, pos)  =  "a" + U+1F335 + line + "-" + pos
```

where U+1F335 is the cactus emoji 🌵. The cactus is the prefix marker; the hyphen `-` separates `line` from `pos` to prevent identity collisions (e.g., distinguishing `(line=1, pos=25)` from `(line=12, pos=5)`).

R-9.2.2. The cactus 🌵 is reserved — it is excluded from the `NAME` token (§2.3), so auto-names cannot collide with user-defined names. (The hyphen `-` is permitted inside `NAME` tokens; it does not need exclusion because the cactus prefix already disambiguates auto-names from user names.)

Example: a `>>` suffix at `line=12, pos=5` emits `@name="a🌵12-5"`.

R-9.2.3. **File-local handles (R-3.10.12).** A `>> name` suffix emits the object with its cactus `@name` **and** a `@local="name"` marker; references stay as plain `<o base='name'>`. The first-pass `resolve-local-names` reshape (right after `wrap-applications` / `resolve-self`, before `build-fqns`) collects the per-file `@local → @name` table and rewrites every `@base` equal to a handle into the matching cactus `@name`; a handle declared twice is reported there as a `resolve-local-names` check error. The `@local` marker is **kept** on the declaring object so that the readable handle can be recovered from the otherwise-synthetic cactus name — in particular by the printer, which prints `? >> name` voids back under their handle rather than a `vL_P` placeholder (#5563). Downstream compilation passes reference the reserved cactus name and ignore the marker.

### 9.3 Source-token to XMIR-character mapping

| Source token | XMIR character | Used as |
| --- | --- | --- |
| `@` (PHI) | `φ` | `@name='φ'` for the @-attribute |
| `^` (RHO) | `ρ` | `@base='ρ'` for parent reference |
| `Q` (ROOT) | `Φ` | `@base='Φ...'` for root-rooted FQNs |
| `$` (XI) | `ξ` | `@base='ξ'` for self reference |
| `%` (SELF) | — | base-less `<o self=''>` marker; `resolve-self` (§9) later sets `@base` to the enclosing anonymous formation's auto-name (§3.15) |
| `T` (TERM) | `⊥` | `@base='⊥'` for the bottom term |
| atom signature head `Q` | `Φ` | `@atom='Φ....'` |
| generic type variable `A`–`F` | (verbatim) | `@atom`, `@type`, `@args` member — never `Φ`-promoted or alias-expanded (§3.10.11) |

### 9.4 Per-construct attribute emission

| Source construct | XMIR effect |
| --- | --- |
| Void parameter `[a b c]` | Each param emits `<o name='<param>' base='∅'/>` as a void child |
| Const-marker `> name!` | `@const` attribute (empty value: `@const=""`) |
| Truthy test attribute `[] +> name` | `@name='+<name>'` (the `+` prefix marks it as a truthy test; transpiles to `Assertions.assertTrue`) |
| Throwing test attribute `[] -> name` | `@name='-<name>'` (the `-` prefix marks it as a throwing test; transpiles to `Assertions.assertThrows`) |
| Atom signature `> name /sig` | A wrapper `<o>` carries the user-given `@name='<name>'`. Children, in order: (1) void params from the `[…]` head, in source order; (2) the marker `<o name='λ' atom='<sig>'/>` immediately after the voids; (3) any test attributes (`+>`) that follow. `<sig>` is a `Φ`-promoted concrete forma **or** a bare generic type variable A–F (verbatim). Example: `[a b] > foo /bar` emits `<o name='foo'><o name='a' base='∅'/><o name='b' base='∅'/><o name='λ' atom='bar'/>...</o>` |
| Void own-type `? > name /type` | `@type='<type>'` on the void's `<o>`: a `Φ`-promoted concrete forma or a verbatim variable A–F, with any trailing `?` preserved (R-3.4.8) |
| Void callback types `? > name /{type …}` | `@args='<type> …'` on the void's `<o>`: space-separated members, each promoted forma or verbatim variable; no `?` (R-3.4.8) |
| Compact tuple `*N` | A synthetic wrapper `<o base='Φ.tuple' star=''>` is emitted as the *last* child of the head, containing the tupled portion of the children. The first `N` children stay as direct positional args; the remaining children go inside this wrapper. The `@star` attribute lives on the wrapper, never on the head. See R-3.9.2 for the partition. |
| Inline binding `:label` | `@as='label'` on the argument `<o>` |
| Inline binding `:N` | `@as='αN'` |
| `.method` line | `@method` attribute (empty); `@base` is prefixed with `.`; `@pos` records the dot column (R-9.1.3) |
| `?.` fragile dispatch (R-3.5.3a) | `@fragile` attribute (empty) added to the dispatch link's `<o>`, alongside its usual emission: a method link keeps `@method=''` and gains `@fragile=''`; a reversed dispatch carries `@fragile=''` without `@method`. `@base` is unchanged (still `.<name>`, not `?.<name>`), so passes matching `starts-with(@base, '.')` are unaffected. `@pos` records the operator's first character (the `?`). |
| Unbound positional arg | `@as` is **not** emitted at parse time. It is added by a later post-parse pass |

R-9.4.1. `@method` and `@as='αN'` are intermediate attributes; they may be transformed or removed by downstream XSL passes (`wrap-method-calls.xsl` and equivalents). The parser emits them; what happens after is out of scope.

### 9.4.2 Per-outer-kind child emission order

For each outer kind, this table fixes the order of `<o>` children inside its emitted element. Two implementations must produce identical sibling ordering.

| Outer kind | Children in this order |
| --- | --- |
| `head` | none (single self-contained `<o>` carrying `@base`). |
| `hmethod` (single line, 0 hargs) | Emits a **sequence of sibling `<o>`s** under the enclosing parent: (1) the receiver of the chain as the first sibling; (2) one `<o base='.<methodname>' method=''>` per `.method` segment, in source order. Because the whole chain lives on **one source line**, only the last `.method` segment can carry a name suffix (the line's `> name`); intermediate segments on the same line have no syntactic place for their own name. (The intermediate-names provision of R-6.2.3 applies only to `vmethod` chains, where each `.method` is on its own line.) See R-9.0.3 for the convention. |
| `happlication`, `vapplication` | The outer `<o>` carries `@base = head reference` (or, for chained heads, the last link's `<o>` is the outer one — R-9.0.3.1). Arguments emit as direct children of that `<o>` in source order. |
| `bare-formation` | (1) void params from the `[...]` head, in source order; (2) all body children — plain, master, and `+>` / `->` test attributes — in source order as they appear in the body. The parser does **not** reorder children by category; the category distinction in §1.3 is about role and naming requirements, not emission order. |
| `inline-phi-formation` | (1) void params from `> [params]` in source order; (2) the LHS expression as a single child with `@name='φ'`. When the LHS has no horizontal args this `φ` `<o>` stays open, and unnamed deeper-indent lines emit inside it as its vertical arguments (`@as='αN'`, per `vapplication`); a named deeper-indent line closes the `φ` `<o>` and emits as a sibling attribute of the formation `<o>` (§4.5 / R-5.3.6); at close time the φ `<o>` (if still open) and the formation `<o>` are closed. When the LHS has horizontal args the φ is closed at emit time and the line accepts no children (R-6.1.1). |
| Atom (formation + `/sig`) | (1) void params in source order; (2) `<o name='λ' atom='<sig>'/>` marker; (3) test attributes (`+>` / `->`) in source order |
| `bare-reversed` (vertical) | (1) the receiver's `<o>` first; (2) each method-arg `<o>` in source order |
| `reversed-with-hargs` | same as `bare-reversed` — receiver first, args in source order |
| `vmethod`, `vmethod-with-hargs` | Same shape as `hmethod` (R-9.0.3): the chain emits as **flat siblings** under the enclosing parent — the receiver first, then one `<o base='.<methodname>' method=''>` per `.method` continuation line in source order. The chain's outermost user-given name attaches to the last link. For `vmethod-with-hargs`, any horizontal args on the final `.method` line appear as children of that final link's `<o>`, in source order. Intermediate optional names (R-6.2.3) attach to their respective link `<o>`s. (Downstream `wrap-method-calls.xsl` folds the flat siblings into nested form — outside the parser's contract.) |
| `compact-tuple` | The outer `<o>` carries `@base = head reference`. Children, in order: (1) the first `N` partitioned children as direct positional args in source order; (2) a synthesised `<o base='Φ.tuple' star=''>` as the **last** child, containing the remaining children in source order (R-3.9.2). |
| `*` (STAR) as head of any kind (`head`, `happlication`, `vapplication`) | When `*` is the head token, the outer `<o>` carries `@base='Φ.tuple'` and `@star=''` regardless of which outer kind the line forms (head alone, happlication with hargs, vapplication with deeper-indent args). The arguments — whether horizontal (`* x y z`) or vertical (the line `*` followed by deeper children) — become direct children of that `<o>` in source order. This emission shape **overrides** the default head emission for `*`-headed lines; the kind classification (head / happlication / vapplication) governs cross-line behaviour but not the head's `@base`. |
| `text-block` | none (the literal text is the `<o>`'s value) |

R-9.4.2. The "source order" of vertical children is the order their head lines appear in the source. Comments interleaved between siblings do not affect arg ordering.

### 9.5 Comment emission

R-9.5.1. The top comment block emits one `<comment>` element inside `/object/comments`, documenting the program. At most one block exists per program.
R-9.5.2. The comment's body is the concatenation of the comment lines with the leading `#` removed from each. Per the `COMMENTARY` token rule (§2.3), a comment line ends on a non-whitespace character — trailing whitespace is already excluded at the token level. The body text is preserved verbatim after the `#`; leading whitespace immediately after the `#` is implementation-defined (typical: strip exactly one space if present).
R-9.5.3. The `<comment>` carries a `line` attribute pointing at the first meta or object that flushed it (the construct it precedes).

### 9.6 Errors

R-9.6.1. A parse error emits one `<error>` element inside `/object/errors`. The element carries `@line`, `@pos`, `@severity` attributes and a text body with the canonical message (§9.9).
R-9.6.2. **`@severity` values.** The parser emits exactly one value: `error` (the parser cannot accept the construct; recovery applies per §7). Every rule violation in this spec carries `@severity='error'`. The `<error>` element schema permits additional values (`warning`, `info`) for use by downstream layers (lints, type-checking, semantic passes), but those are out of the parser's scope — the parser produces only `error`. Implementations parsing this spec need not generate `warning`/`info`; readers of XMIR from any source should accept those values without surprise.
R-9.6.3. Recovery proceeds per §7.

### 9.7 String and text literals

R-9.7.1. The `STRING` token is delimited by `"` and may contain:
- Any character except an unescaped `"`, backslash, carriage return, or newline.
- The escape sequences in §9.7.3.

R-9.7.2. The `TEXT` token is delimited by `"""` markers (the opening `"""` must be at the end of a line; the closing `"""` must be on its own line at the opening indent). Body characters: any character except an unescaped backslash, plus the escape sequences in §9.7.3.

R-9.7.3. **Escape sequence table.** Recognised in both `STRING` and `TEXT`:

| Sequence | Meaning |
| --- | --- |
| `\b` | U+0008 (backspace) |
| `\t` | U+0009 (tab) |
| `\n` | U+000A (newline) |
| `\f` | U+000C (form feed) |
| `\r` | U+000D (carriage return) |
| `\"` | `"` |
| `\'` | `'` |
| `\\` | `\` |
| `\NNN` | Octal byte. `N` ∈ `[0-7]`, length 1–3 digits, value ≤ 0o377 (= 255 decimal). Pattern: `\\` followed by an optional `[0-3]`, an optional `[0-7]`, and a required `[0-7]` |
| `\uXXXX` | Unicode codepoint, 4 hex digits. The grammar permits `\uu...uXXXX` (one or more `u`s) for legacy escape forms; the parser recognises any such sequence and decodes it to the codepoint. |

R-9.7.4. **Escape decoding happens at parse time.** Every recognised escape — single-character, octal, and unicode — is decoded into its target codepoint(s) by the parser before the string body is fed into the `<o base='Φ.bytes'>` UTF-8 carrier (R-9.4 data carrier emission). The XMIR text body therefore contains decoded characters, never the source-level escape sequence; this applies equally to `STRING` and `TEXT` tokens (R-9.7.1 / R-9.7.2). Downstream consumers see the canonical UTF-8 bytes, not the literal `\uXXXX` / `\NNN` form.

Any other backslash sequence is a lexical error.

### 9.8 Numeric literals

R-9.8.1. `INT`: optional sign (`+` or `-`), then either `0` alone (the literal zero) or a digit in `[1-9]` followed by any number of digits in `[0-9]`. **Any leading zero on a multi-digit literal is forbidden** — `07`, `007`, `+07`, and `-07` are all lexical errors. The new parser narrows the underlying grammar here: the grammar (`INT : (PLUS | MINUS)? (ZERO | ZERO?[1-9][0-9]*)`) permits one optional leading zero before a non-zero digit-run; the new parser does not. Implementations must check explicitly after lexing.
R-9.8.2. `FLOAT`: optional sign, one or more digits, `.`, one or more digits, optional exponent `(e|E)(+|-)?digits`.
R-9.8.3. `HEX`: literal `0x` (lowercase only) followed by one or more hex digits (case-insensitive).

### 9.9 Error messages — canonical texts

Two parsers should report the same condition with identical message strings for deterministic golden-file testing.

R-9.9.1. Every error condition in this spec has a single canonical text — **including lexical and indent-related errors** (e.g., `unexpected odd indent`, `tab character in leading whitespace`, `invalid signed-number literal`), not only parse-phase errors. The table below assigns one to each. Implementations must use the exact string (with the position prefix as the only variable part).

| Condition | Canonical message |
| --- | --- |
| Odd indent | `unexpected odd indent` |
| Indent jump > 1 level | `indent increased by more than one level` |
| Tab in leading whitespace | `tab character in leading whitespace` |
| Deeper-indent under horizontally-completed line | `unexpected deeper-indent line — previous expression is closed for children` |
| `.method` continuation on horizontally-completed previous | `method continuation not allowed after horizontal application` |
| `.method` continuation on an only-phi formation | `method continuation not allowed after only-phi formation` |
| `.method` line at top level, deeper than parent, or with no same-indent sibling | `method continuation has no expression to attach to` |
| Chained inline-phi suffix `expr > [a] > [b] > name` | `chained inline-phi suffixes are not allowed` |
| Inline-phi without a name on the right (`expr > [params]` alone) | `inline-phi formation must carry a name on the right` |
| Wrong number of blank lines after meta header (expected exactly one) | `meta header must be followed by exactly one blank line` |
| Blank line(s) before the meta header (file must start with metas, no leading blanks) | `meta header must appear at the top of the file` |
| Leading or trailing space inside `[ ]` (R-3.4.4) | `formation brackets must not contain leading or trailing space` |
| Double space between parameter names in voids (R-3.4.5) | `parameter names in voids must be separated by exactly one space` |
| More than one space between meta parts (R-3.2.4) | `meta parts must be separated by exactly one space` |
| Test attribute name is `@` (PHI) instead of NAME (R-6.3.5) | `test attribute name must be an identifier, not @` |
| Leading-zero in integer literal (R-9.8.1, e.g., `007`) | `integer literal must not have leading zeros` |
| Decimal `INT`/`FLOAT` literal whose exact decimal value differs from the IEEE-754 double it parses to (dead digits; e.g., `2.7182818284590452354`). Alternate spellings of the same value (`+42`, `1.50`) are accepted. | `<literal> is over-precise, write <canonical> instead` (literal and canonical substituted) |
| `+` followed by digit but the digit-run forms an invalid `INT`/`FLOAT` (R-3.2.5, e.g., `+1foo`) | `invalid signed-number literal` |
| Nested atom inside another atom (R-6.3.4 (b)) | `atom may not contain a nested atom` |
| `[x]` as a bare argument (horizontal anonym in arg position) | `horizontal formation not allowed as argument` |
| Malformed BYTES literal (R-3.13.1 — invalid byte form, e.g., `Z9-`, single trailing dash without prefix, odd hex run) | `invalid bytes literal` |
| Meta after first non-meta object | `meta directive must precede all other objects` |
| Plain child without name in formation | `object inside formation must have a name` |
| Atom containing non-test child | `atom may contain only test attributes` |
| `+>` outside indent 2 of top level | `test attribute legal only as direct child of top-level object` |
| Bare reversed dispatch missing receiver | `reversed dispatch missing receiver` |
| Bare reversed dispatch receiver starts with `.` | `reversed dispatch receiver must not begin with dot` |
| Compact tuple `*N` with N > children | `compact tuple requires at least N children, got K` (N and K substituted) |
| Compact tuple `*0` on reversed dispatch | `reversed-dispatch compact tuple requires N ≥ 1` |
| Mixed bindings in arg group | `argument bindings must be all-or-nothing` |
| Receiver carrying binding | `reversed-dispatch receiver cannot carry a binding` |
| Inline binding on non-last method in chain | `inline binding allowed only on the last method in a chain` |
| Comment not on top of the file | `comment is allowed only on top of the file, before metas` |
| Blank line inside the top comment block | `blank line inside the top comment block is not allowed` |
| No blank line after the top comment block | `a blank line must separate the top comment block from the rest of the file` |
| Blank line between non-master siblings | `blank line not allowed between non-master siblings` |
| Unclosed text block | `unclosed text block opened at line N` (N substituted) |
| Multi-line BYTES continuation indent shallower than opener | `multi-line bytes continuation must not de-indent` |
| Multi-line BYTES interrupted by non-bytes content | `multi-line bytes interrupted by non-byte content` |
| Const + atom | `const and atom signature cannot be combined` |
| `>>` with `/sig` | `auto-named atom is forbidden` |
| Type variable outside A–F | `type variable must be one of A-F` |
| `?` optional marker on an atom return signature | `optional marker ? is allowed only on a void attribute` |
| `?` inside a `/{…}` argument list | `? is not allowed inside a /{…} argument list` |
| Type annotation on a void outside an atom | `a void type annotation is allowed only inside an atom` |
| Two type annotations on one void | `a void attribute may carry at most one type annotation` |
| Unexpected odd character after a name suffix | `unexpected content after name suffix` |
| Excessive trailing blank lines | `more than one trailing blank line` |

R-9.9.2. The position prefix `[L:P]` (with L = line, P = pos) is prepended to every error message before insertion into the `<error>` element body. Conventions: 1-indexed line, 0-indexed pos.

R-9.9.3. New error conditions added to the spec must extend this table with a canonical text.

---

## Appendix A — Outer kinds reference

**Notation:** a kind's *openness* (open / vertical-completed / horizontal-completed) is a separate dimension from its kind name. The same kind progresses through openness states as more lines arrive; the kind name itself does not change. Below, "after block closes" means the kind's openness has transitioned to `vertical-completed`.

| Outer kind | Cardinality | Open for deeper children? | Wrappable by same-indent `.method`? | Notes |
| --- | --- | --- | --- | --- |
| `head` | 1 line | yes | yes (after) | bare identifier / literal / group, no chain, no hargs |
| `hmethod` | 1 line | yes (if 0 hargs) | yes (after) | `x.y.z` chained dispatch with 0 hargs only |
| `happlication` | 1 line | **no** | **no** | head + ≥1 horizontal args. Subsumes "hmethod with hargs" (the chained head doesn't change the outer kind). |
| `bare-formation` | 1 line + body | yes (body) | yes (after) | `[params] [> name]` |
| `bare-reversed` | 1 line + body | yes (receiver+args) | yes (after) | `name.` vertical form |
| `reversed-with-hargs` | 1 line | **no** | **no** | `name. arg1 arg2…` |
| `compact-tuple` | 1 line + body | yes (tuple+direct args) | yes (after) | `head *N` |
| `inline-phi-formation` | 1 line (+ body if bare φ) | yes if φ has 0 hargs (φ's vertical args, plus named sibling attributes); **no** if φ has hargs | **no** | `expr > [params] > name`; a bare φ opens for a vertical-argument block, and named body lines are the formation's own attributes (§4.5 / R-5.3.6) |
| `vapplication` | multi-line | yes while in progress | yes after block closes | head + vertical block |
| `vmethod` | multi-line | yes while in progress (only if last `.method` has 0 hargs) | yes (more `.method`s extend the chain; same-indent `.method` after block closes wraps it) | chain of `.method` continuations |
| `vmethod-with-hargs` | multi-line | **no** | **no** | a `.method` continuation line that itself carries ≥1 horizontal args — closes the chain immediately |
| `pipe-application` | 1 line (+ body if 0 hargs) | yes if 0 hargs (vertical form's args) | yes (after) | `\| [args] [> name]`; applies args to the same-indent named formation/pipe above (§3.14) |
| `text-block` | multi-line | n/a | yes (after closing `"""`) | `"""…"""` |

**Horizontally-completed kinds (the single source of truth)** — these never receive deeper children and cannot be wrapped by same-indent `.method`:

```
{ happlication, reversed-with-hargs, vmethod-with-hargs }
```

R-5.2.3 and R-6.1.1 reference this set; if the set changes, change it here, not in those rules. An `inline-phi-formation` is **not** a member: its openness depends on its φ (R-6.1.4) — a bare φ is `open` (but still not wrappable by `.method`, §5.2.3(b′)), a φ with hargs is `horizontal-completed`.

## Appendix B — Per-line shape decision table

Reproduced from §3.1 for convenience:

| First non-space char | Lookahead | Shape |
| --- | --- | --- |
| `+` digit | — | signed-number app (§3.6) |
| `+` | otherwise | meta (§3.2) |
| `#` | — | comment (§3.3) |
| `.` | — | method-dispatch line (§3.5) |
| `\|` | — | pipe-application line (§3.14) |
| `[` | — | formation (§3.4) |
| `"""` alone | — | text-block opener (§3.11) |
| identifier | `.` followed by space/EOL | reversed dispatch (§3.8) |
| identifier | otherwise | application or just-an-identifier (§3.6) |
| literal | — | application (§3.6) |
| `*` | — | star tuple (§3.6) |
| `(` | — | group application (§3.6) |

## Appendix C — Worked examples

### C.1 Mixed-construct walkthrough

```
# A program demonstrating most constructs.        ← R-3.3: top comment block, before metas
# Comment block of two lines.
                                      ← R-6.5.2: required blank after the comment block
+author someone@example.com           ← R-3.2: meta at indent 0
+version 1.0                          ← meta
                                      ← R-6.5.5: required blank after metas
[args] > main                         ← R-3.4: top-level formation
  mem.alloc > @                       ← R-3.6: hmethod head (0 hargs), named @; becomes vapplication when children push
    0                                 ← R-5.2.7: deeper child at indent 4, parent=hmethod (not formation), no name required
    [x] >>                            ← R-3.4: nested formation with auto name >>
      run > @                         ← R-3.6: head, named @; vapplication
        *                             ← R-3.6: star tuple as head; becomes vapplication
          x.put 2                     ← R-3.6: happlication, horizontally-completed (R-6.1.1)
          loop                        ← R-3.6: bare head
            x.lt 6 > [i] >>           ← R-3.10 inline-phi: outer = inline-phi-formation; φ `x.lt 6` has a harg, so closed for deeper children (§4.5)
            [i] >>                    ← R-3.4: formation
              true
```

### C.2 Stack trace for a vmethod naming check

Source:

```
[] > foo
  tmpdir
  .tmpfile
  .open > @
```

Stack evolution:

| After line | Stack (bottom→top) | Notes |
| --- | --- | --- |
| `[] > foo` | `[formation@0{kind=formation, parent=top-level, named?=true, open}]` | bare-formation pushed at indent 0 |
| `  tmpdir` | `[formation@0, head@2{parent=formation, named?=false, open}]` | head pushed at indent 2; parent kind = formation, so naming will be required at close |
| `  .tmpfile` | `[formation@0, vmethod@2{parent=formation, named?=false, open}]` | same-indent `.method`, top kind extends to vmethod; still no name |
| `  .open > @` | `[formation@0, vmethod@2{parent=formation, named?=true, open}]` | another `.method` continuation; `named?` flips true (suffix `> @`) |
| EOF | pop indent-2 entry: parent=formation, named?=true → OK; pop indent-0 entry: parent=top-level, named?=true → OK | Program accepts |

If `> @` were absent on `.open`, the close-time check (R-5.3.1) at the second pop would error "object inside formation must have a name" at the start line of the vmethod chain.

### C.3 Stack trace for atom-body restriction

Source:

```
[] > foo /number
  a > bad-child
```

| After line | Stack | Notes |
| --- | --- | --- |
| `[] > foo /number` | `[atom@0{kind=formation+atom, parent=top-level, named?=true, open}]` | atom pushed at indent 0 |
| `  a > bad-child` | `[atom@0, head@2{parent=atom, named?=true, open}]` | plain child pushed under atom |
| EOF | pop indent-2: R-5.3.4 fires — `parent=atom`, but the popped kind is not `formation + +> suffix` → error "atom may contain only test attributes." | |

### C.4 Reversed dispatch with horizontal args

```
[args] > main
  [y] > leap
    or. > @
      and.
        eq. (mod. y 4) 0
        not. (eq. (mod. y 100) 0)
      eq. (mod. y 400) 0
  out. > @
    format
      "%d is %sa leap year!"
      (args.at 0).as-int > year!
      if (leap year:y) "" "not "
```

Line-by-line classification highlights:

| Line | Kind | Outer | Notes |
| --- | --- | --- | --- |
| `[args] > main` | Formation | bare-formation | top-level master |
| `  [y] > leap` | Formation | bare-formation | master child of `main`, named `leap` (R-1.3) |
| `    or. > @` | ReversedDispatch | bare-reversed | vertical form, named `@`; opens block awaiting receiver + args |
| `      and.` | ReversedDispatch | bare-reversed | first deeper child = receiver of `or.`'s outer arg (R-5.2.8); itself opens nested receiver block |
| `        eq. (mod. y 4) 0` | ReversedDispatch | reversed-with-hargs | **horizontal** form: receiver = `(mod. y 4)` (paren group), method arg = `0`. Line is horizontally-completed (R-6.1.1) |
| `        not. (eq. (mod. y 100) 0)` | ReversedDispatch | reversed-with-hargs | horizontal form, single arg (the receiver) — `.not` is a unary method |
| `      eq. (mod. y 400) 0` | ReversedDispatch | reversed-with-hargs | second arg of `or.`'s vertical block |
| `  out. > @` | ReversedDispatch | bare-reversed | vertical form, named `@` |
| `    format` | Application | head | bare head, becomes vapplication once children push |
| `      "%d is %sa leap year!"` | Application | head | string literal as first vapp arg of `format` |
| `      (args.at 0).as-int > year!` | Application | hmethod | paren group is the head, `.as-int` chains; name `year!` (const-marked) |
| `      if (leap year:y) "" "not "` | Application | happlication | head `if`, three horizontal args: `(leap year:y)` (paren group with binding `:y` inside), `""`, `"not "` |

Notable points:

- **Horizontal reversed dispatch (`eq. (mod. y 4) 0`)** appears 4× in this snippet. R-3.8.2 and R-6.6.3 govern it.
- **Receiver as paren group**: `(mod. y 4)` is the receiver of `.eq`. Per R-3.8.2, the first horizontal arg is always the receiver.
- **`mod. y 4`**: receiver = `y` (just an identifier — receivers don't have to be paren groups), method arg = `4`. Same as `y.mod 4` in regular notation.
- **`year!` on a vapp arg**: a vapplication's vertical args may each carry an independent name suffix. This is orthogonal to the all-or-nothing binding rule (R-6.6.2 governs `:label` bindings, not `> name` suffixes). A named direct body line of an only-phi formation is not such an argument at all — it is a sibling attribute of the formation (§4.5 / R-5.3.6); a name suffix deeper inside a φ argument is an ordinary named vertical argument and behaves exactly as here.
- **`:y` binding on `leap year:y`**: inside the paren group, `leap` is applied to `year` with binding `:y`. Single-arg application, all-or-nothing trivially satisfied.

This snippet exercises hmethod chains, horizontal reversed dispatch, paren groups, vapplication, name suffixes with `!`, and `:label` bindings — every major construct except text blocks and compact tuples.

---

## End

This spec is complete in the sense that every cross-line predicate reduces to operations on the indent stack (§5), and every per-line shape produces a LineShape record (§3) that feeds those operations. Implementation skeleton (the three-method classify / validate / emit shape) is the next artefact.
