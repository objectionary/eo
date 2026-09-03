# Issue 8249: Parenthesized Inline-Phi Compact Tuple Design

## Goal

Prevent a compact-tuple marker on the left-hand side of a parenthesized
anonymous inline-phi from being emitted as an empty tuple argument.

## Chosen behavior

Reject a syntactically valid trailing compact-tuple marker (`*` or `*N`) when
it appears on the body of an anonymous inline-phi inside parentheses. A
parenthesized expression cannot provide the deeper-indented children that a
compact tuple partitions, so accepting the marker would either invent an
argument or silently discard the marker and its count.

The parser will report the error at the compact-tuple marker with the message:

```text
compact tuple marker is not allowed inside a parenthesised inline-phi
```

Ordinary star arguments outside this specific inline-phi position remain
unchanged.

## Alternatives considered

1. Strip the marker and emit the bare phi. This matches the current no-child
   result of line-level only-phi for `*`, but it cannot honor a positive count
   such as `*1` and silently ignores source syntax.
2. Synthesize an empty tuple argument. This is the current defect because it
   changes the phi from a bare object into an application and diverges from
   line-level only-phi handling.
3. Reject the marker. This is the selected option because it preserves a
   single unambiguous meaning for compact-tuple syntax.

## Implementation

`LnOnlyPhi.compactStar(String, Span)` remains the source of truth for
recognizing a valid compact-tuple suffix, including parenthesized heads and
count-overflow validation. Its visibility will be relaxed from private to
package-private so `Emissions.inlinePhi` can reuse it without duplicating the
grammar.

After `Emissions.inlinePhi` extracts the left-hand side and constructs its
source-aligned `Span`, it will call that detector. A detected marker raises a
`ParseError` before any object is emitted. All other inline-phi input follows
the existing emission path.

## Tests

A YAML parser pack will reproduce `bar (seq * > [m]) > z` and assert that the
parser reports the new diagnostic. The focused `EoSyntaxTest` and
`LnOnlyPhiTest` suites will be run first, followed by the complete
`eo-parser` test suite to protect line-level compact tuples and ordinary star
arguments from regressions.
