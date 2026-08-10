# Merge-monikers Hot-path Optimization Design

## Context

Pull request #6511 replaced the quadratic subtree scans in
`merge-monikers.xsl` with keys. Issue #6512 also identifies three remaining
constant-factor costs in the same stylesheet: repeated pure-function calls,
expensive function predicates evaluated for irrelevant nodes, and sorting a
dispatch sequence when it has fewer than two items.

This change addresses only those three local optimizations. Auditing sibling
stylesheets and replacing the bidirectional binding/reference lookups with a
single mapping pass remain outside this pull request.

## Design

The stylesheet will keep XSLT 2.0 and move the hosted/applied binding lookups
out of template match patterns. Each guarded template will compute its binding
once in a local variable, process it when present, and call `xsl:next-match`
when absent. This removes the duplicate pattern/body evaluation without a
transformation-wide cache.

The template patterns that call `eo:hosted-binding` and `eo:applied-handle`
will begin with inexpensive predicates derived from each function's necessary
conditions. A node must carry a local-reference base beginning with `$eo:xi-dot`
before either function can return a binding. Applied handles additionally need
child arguments and no name. These predicates reject unrelated nodes before
Saxon evaluates the expensive function predicate.

`eo:moniker-refs` will collect dispatch candidates without sorting them first.
If the sequence has zero or one item, it will be returned directly. Only a
sequence containing a second candidate will enter `xsl:perform-sort`. The
existing segment-count ordering and document-order tie behavior will remain
unchanged for sequences that are actually sorted.

## Compatibility and Failure Handling

Saxon-HE 13.0 supports function caching, but a synthetic 1000-binding
measurement showed that it is counterproductive after #6511 made the function
bodies cheap key lookups. The unchanged sheet measured 82.118 ms; both caches
measured 91.436--114.516 ms; removing both caches while retaining the other
optimizations measured 81.583 ms. Caching either function independently was
also slower. The design therefore removes repeated evaluation structurally
instead of retaining results for the lifetime of a transformation.

The changes do not introduce a new runtime failure path. Stylesheet compilation
under the project's pinned Saxon version is covered by the existing printer
tests. A compilation error or an output difference will fail the build.

## Testing

A new structural regression test will read `merge-monikers.xsl` and assert the
optimization contract: lookup functions absent from match patterns, local
binding variables evaluated once in guarded templates, `xsl:next-match`
fallbacks, and a conditional sort. The test will be run before implementation
to confirm it fails for the missing optimizations.

After implementation, the focused regression test and the complete
`eo-printer` test suite will run. Existing XMIR fixtures cover output semantics,
including moniker selection, dispatch ordering, applied handles, const handles,
and local handles. A local synthetic transformation will supplement the test
suite with before/after timing data without adding a machine-dependent timing
threshold to CI.

## Acceptance Criteria

1. The focused regression test fails before the stylesheet change and passes
   afterward.
2. Hosted and applied binding lookups execute once in each selected template,
   rather than once in its match pattern and again in its body.
3. Irrelevant nodes are rejected by cheap template patterns before the
   hosted/applied lookup functions run.
4. Dispatch sorting runs only for two or more candidates and preserves the
   existing ordering when it runs.
5. All `eo-printer` tests pass with no output-behavior regressions.
6. The pull request changes no sibling stylesheet and performs no mapping
   refactor.
