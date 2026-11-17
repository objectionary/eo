# One width guard for `number` (#9893)

The eight-byte check is written five times (`sqrt.eo:26`, `ln.eo:25`,
`as-char.eo:30`, `power.eo:33`, `mod.eo:42` and `:47`) in two wordings, so a
wrong-width receiver blames `'ρ'` or `'b'` — an attribute the caller never
wrote (#9283). Untouched: `is-finite`, `is-nan` and `eq`; `cosine`, `exp`,
`floor` and `power`'s receiver keep failing at `BytesRaw.whenFit`.

## Decision

One attribute, `number/checked.eo`:

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
`b.checked`) instead of building their own `if.`. The message states the
width, not the attribute, in the `u64.eo:24`/`i64.eo:43` house form.

## Alternatives

A guard per object keeps the message next to the blame, but the five copies
drift. Guarding every attribute widens the change into `is-nan`/`is-finite`,
whose tests assert `false` for a wrong width. Cost if wrong: one additive
file and five dispatches, all reversible.
