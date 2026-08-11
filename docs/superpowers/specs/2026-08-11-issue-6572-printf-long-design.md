# Issue #6572: long-range decimal formatting

## Context

[`string.printf`](https://github.com/objectionary/eo/issues/6572) documents `%d` around the signed 64-bit range, and both `%d` and `%f` ultimately render integer digits through `string.as-decimal`. The old implementation rejected every magnitude at or above `2^53` because its floating-point digit loop could emit incorrect digits. Merely widening that guard is unsafe: the exactly representable value `38802277692848472`, for example, is misrendered by repeated floating-point division and subtraction.

The committed regressions establish the desired signed-long contract. In the focused baseline, all 30 `as-decimal` seam, range, and truncation cases pass with the exact pure-EO split, as does signed-minimum `%d`; the only errors are signed-minimum `%f` and `%.0f`, both caused by `string.as-fixed` negating `Long.MIN_VALUE` to the out-of-range positive value `2^63`.

## Exhausted pure-EO approaches

The signed-long algorithms were correct but failed the formatter's production latency requirement:

- Repeated exact `i64.div 10` caused five `EOas_decimalTest` cases and one `EOprintfTest` case to reach the existing 20-minute JUnit timeout. The other 64 focused cases passed, isolating the failure to conversion cost rather than decimal semantics.
- One exact signed `i64.div 1_000_000_000` per large value produced correct digits but required 58–88 seconds for isolated large cases and hundreds of seconds for contended suite cases.
- Bounded-quotient and chunk-rendering variants removed general full-width division. Exhaustive isolated trials still measured 21.890–23.386 seconds for the fastest variant, while the other variants measured 27–88 seconds.

Every pure-EO route therefore exceeds the explicit `<10s` isolated-test gate, including the fastest result by more than two times. Further micro-optimization of the same object graph has no evidence of reaching the gate. The narrow JVM atom below is an exception justified by measured latency, not by convenience.

The repository already accepts Java-only atom changes, including the recent `dataized` and `recovered` atoms after the `eo2js` repository was archived. The Maven build, generated atom metadata, and current CI validate the JVM runtime only. This precedent makes a JVM atom an appropriately scoped fix for issue #6572.

## Scope

This change will:

- render every finite value accepted by the signed 64-bit contract through `%d`;
- allow `%f`, `%.2f`, and `%.0f` to render large integer-valued doubles below positive `2^63`;
- preserve truncation toward zero in `string.as-decimal`, including negative fractions that truncate to unsigned zero;
- preserve deliberate failures for non-finite and out-of-range values;
- render `Long.MIN_VALUE` without negating the whole value;
- add direct JVM coverage of exact signed-long decoding, UTF-8 decimal output, wrong-width rejection, and the declared `Q.bytes` result;
- retain all committed EO range, seam, and `printf` regressions.

This change will not alter production code in `string.printf`, address the separate one-ULP rounding artifact reported for `printf "%.2f" 123456789012345.67`, or implement the corresponding Node atom.

## Selected architecture

`string.as-decimal` remains the sole decimal-integer rendering primitive used by `%d` and `string.as-fixed`. It owns all semantic guards and converts the accepted `number` exactly once:

1. Lazily reject a non-finite value before evaluating range conversion.
2. Reject `n >= 2^63`.
3. Reject `n < -2^63`.
4. Evaluate and cache the sole `n.as-i64` expression as `integer!`.
5. Redecorate those cached bytes as `i64 integer` at the typed atom application.
6. Return the atom's signed decimal UTF-8 bytes directly.

There is no whole-value absolute operation, negation, quotient estimate, chunk split, exact division, multiplication, or EO digit recursion on the accepted value. `number.as-i64` continues to define truncation toward zero, so `-0.9` becomes the eight-byte representation of zero and the atom emits `0`. `Long.MIN_VALUE` is passed unchanged.

### EO API

The nested atom is private to `string.as-decimal` and has this exact signature:

```eo
[] > from-i64 /Q.bytes
  ? > value /Q.i64
```

The guarded call uses the cached bytes with their `i64` decoration restored:

```eo
from-i64
  i64 integer
```

`as-decimal.eo` declares the standard runtime metadata used by existing atom EO sources:

```eo
+rt jvm org.eolang:eo-runtime:0.0.0
+rt node eo2js-runtime:0.0.0
```

The Node locator is retained because all current atom EO sources declare both runtimes; it does not imply that this branch supplies a Node implementation.

### JVM API and data path

The implementation is `org.eolang.EO_string.EOas_decimal$EOfrom_i64`, annotated as `as-decimal.from-i64`, extending `PhDefault`, and implementing `Atom`. Its constructor exposes one void attribute named `value`. Its complete conversion is:

```java
final long value = new Dataized(this.take("value")).take(Long.class);
return new Data.ToPhi(new BytesOf(Long.toString(value)).take());
```

`Dataized.take(Long.class)` requires exactly eight bytes and decodes them as a signed Java `long`; wrong-width input raises `ExFailure`. `Long.toString(long)` is locale-independent and handles both signed extrema without an intermediate negation. `BytesOf(String)` produces UTF-8, and `Data.ToPhi(byte[])` returns an object compatible with the declared `Q.bytes` forma.

The atom must never return `new Data.ToPhi(value)` or any other `Number`. `Data.ToPhi(Number)` converts through `doubleValue()`, which would destroy precision above `2^53` and defeat the purpose of the atom.

### `string.as-fixed`

Places validation remains first. After it succeeds, one narrow helper handles a finite negative value whose magnitude is at least `2^53`. Such a value is integral on the IEEE-754 grid, so the helper passes the signed `n` directly to `as-decimal`. It returns only those digits for `places = 0`; otherwise it appends `.` and exactly `places` zeroes. The helper never evaluates `n.times -1`.

All smaller magnitudes retain the existing rounding path and sign-dropping behavior for values that round to zero. The shortcut is explicitly finite, so `nan` and infinities continue through the deliberate existing non-finite path.

No production branch is added to `string.printf`; its width, zero-padding, precision parsing, and failure propagation remain unchanged.

## Error behavior

- `nan`, positive infinity, and negative infinity terminate with the existing non-finite decimal error.
- Positive values at or above `2^63` and values below `-2^63` terminate before `number.as-i64` is invoked.
- `%d` retains its existing range diagnostic.
- `%f` continues to propagate decimal conversion failure through `string.as-fixed` for an unsupported integer part.
- A direct atom application with anything other than eight bytes raises `ExFailure` during `take(Long.class)`.
- No fallback rounds an out-of-range value, emits partial digits, or converts through `double` after the `i64` boundary.

## Node parity debt

This branch intentionally implements only `EOas_decimal$EOfrom_i64.java`. The archived `eo2js` runtime would require a separate `string/as-decimal$from-i64` JavaScript atom using `BigInt` decoding and string output. Without that file, executing this object under Node will fail atom lookup even though the conventional `+rt node` metadata remains present.

That cross-runtime gap is explicit debt and must be tracked and resolved in a separate Node-parity issue against the archived/runtime successor project. It is excluded here because it requires changes outside this repository, package publication, and restored parity testing. This JVM-only branch must not be presented as Node-compatible.

## Tests and performance gates

Direct Java tests are written and committed before the Java atom exists. They cover `Long.MIN_VALUE`, `Long.MAX_VALUE`, zero, ordinary positive and negative values, `38802277692848472`, rejection of a non-eight-byte input with `ExFailure`, and a returned forma of `Φ.bytes`. The same direct test and focused EO classes run once with `-Deo.typing=true` so the `/Q.bytes` declaration is checked.

Validation proceeds in increasing scope under JDK 21:

1. Run the direct Java atom test.
2. Run `EOas_decimalTest#can_write_the_long_minimum` alone. It must pass and its Surefire testcase duration must be less than ten seconds; the timeout is not weakened.
3. Run the positive and negative base-seam methods and the largest-positive/signed-minimum pair.
4. Run exactly `EOas_decimalTest`, `EOas_fixedTest`, and `EOprintfTest` and record their counts and timings.
5. Run the direct atom test plus those three focused classes with `-Deo.typing=true`.
6. If practical, run the module unit suite and record any unrelated Windows `SyscallTest` baseline failure without weakening or hiding it.

## Acceptance criteria

- All issue examples below positive `2^63` produce the exact expected decimal string.
- The known old-algorithm counterexample and both signed extrema are exact.
- Existing small-number formatting, truncation, padding, precision, and deliberate failures remain unchanged.
- A negative fraction truncating to zero renders `0`.
- The direct atom rejects wrong-width input and returns UTF-8 bytes with forma `Φ.bytes`.
- The isolated signed-minimum testcase is below ten seconds.
- All focused classes pass in ordinary and typing modes.
- Production changes are limited to `as-decimal.eo`, `as-fixed.eo`, and the new Java atom; `printf.eo` contains only its already committed tests.
- The JVM-only exception and separate Node parity debt remain explicit in the design, plan, and review handoff.
