# One eight-byte width guard for `number` (issue 9893)

**Spec:** issue #9893.

## Problem

`number` accepts any bytes, so `number 01-02` and `number "abc"` are legal
objects while every attribute that needs a real double checks the width itself.
The same guard is copied into five objects, in six places and in four shapes
(`number/sqrt.eo:26`, `number/ln.eo:25`, `number/as-char.eo:30`,
`number/power.eo:33`, `number/mod.eo:42` and `:47`), and it is not applied at
all in the rest of the package (`cosine`, `exp`, `floor`, `power`'s receiver):
those reach a Java atom and fail with `BytesRaw.whenFit`.

## Decision

One attribute of the package, `number/checked.eo`:

```eo
[^num] > checked
  if. > @
    num.as-bytes.size.eq 8
    number num.as-bytes
    T
      "A number needs exactly 8 bytes, got %d".printf
        * num.as-bytes.size
```

`sqrt`, `ln`, `mod`, `power` and `as-char` read it (`num.checked`,
`b.checked`) instead of building their own `if.`.

- The message copies the house form of `u64.eo:23` and `i64.eo:43`
  (`"A u64 needs exactly 8 bytes, got %d"`), which states the culprit by
  width instead of naming an attribute. One message, as the issue asks.
- The role of the named holders (`arg` in `sqrt`/`as-char`, `n` in `ln`,
  `expo` in `power`) stays: they keep the checked value as the *receiver* of
  the following comparisons, so the failure stays in `checked` instead of
  being blamed on the `b` of an atom the caller never wrote (#9283).
- `mod` binds `b.checked` to `divisor` and `num.checked` to `dividend`, so its
  two inline guards disappear with the two constructions that motivated them.
- Scope: only the five objects the issue names. `is-finite` and `is-nan`
  keep answering `false` for a wrong width, `eq` keeps comparing raw bytes, and
  the atoms (`plus`, `times`, `div`, `gt`) keep their Java message. The
  receiver of `power` is not checked today and is not checked after this
  change.

## Cost if wrong

The change is additive and reversible: one new file, and inside five files
each guard is replaced by a dispatch to it. Visible difference is the text of
the termination message for a wrong-width receiver — it used to be
`the 'ρ' attribute must be a number` (or `'b'`) and becomes the width
statement. No test in the repository asserts the old text for these objects
(only `as-char`'s `-->` test, which asserts termination, not the message),
and the Java-side texts in `Numeric`/`Int`/`Natural` are untouched.

## Tests

`number/checked.eo` holds the behaviour tests, as `CLAUDE.md` requires:
a positive test that `42.checked` is `42`, and a negative one (`-->`) that
`(number "abc").checked` terminates. The negative test moves from
`as-char.eo:74` (`as-char. --> stops-on-a-code-that-is-not-a-number`), whose
guard is gone; `counts.yml` therefore sees the same number of `-->` markers.
