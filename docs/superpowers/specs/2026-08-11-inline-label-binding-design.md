# Inline Label Binding Design

Issue: [#6563](https://github.com/objectionary/eo/issues/6563)

## Problem

The printer stores an object's `:label` in the line-tree node's `tail`.
`Pretty.horizontal()` currently emits the node's base, then its inlined
arguments, and finally that tail. For `b` with argument `c` and label `lbl`,
this produces `b c:lbl`; reparsing binds `lbl` to `c` instead of `b`.

## Considered Approaches

1. Reject horizontal layout for an application whose tail starts with `:`.
   The existing vertical layout then emits `b:lbl` with `c` below it. This is
   the smallest change and uses an output form explicitly accepted by #6563.
2. Reorder the label before horizontal arguments. This would produce
   `b:lbl c`, but introduces another canonical surface form that is not the
   issue's requested output and needs broader grammar validation.
3. Split labels from other suffixes in the line-tree model and teach the
   printer to reconstruct `(b c):lbl`. This preserves compact spelling but is
   substantially broader and risks exceeding the project's 200-line PR limit.

Approach 1 is selected.

## Implementation

Add a printer fixture for `a (b c):lbl > x` that expects the safe vertical
form. Confirm that it fails because the current printer emits `b c:lbl`.
Then make `Pretty.horizontal()` return no horizontal candidate when the node
has children and its tail begins with `:`. The penalty selector will retain
the existing vertical candidate without changing unrelated suffix handling.

## Verification

The new fixture must pass both exact-output and reprint/idempotency checks in
`XmirTest`. Run the complete `eo-printer` module build with its dependencies,
then run Qulice for the affected module. The final PR must remain below 200
changed lines.
