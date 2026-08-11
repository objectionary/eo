# Issue #6572: long-range decimal formatting

## Context

[`string.printf`](https://github.com/objectionary/eo/issues/6572) documents `%d` around the signed 64-bit range, but both `%d` and `%f` delegate integer rendering to `string.as-decimal`. That object currently rejects every magnitude at or above `2^53`, so exactly representable `number` values between `2^53` and `2^63` fail before formatting.

The `2^53` guard was introduced to prevent the existing floating-point digit extraction from emitting incorrect or non-digit bytes. Removing or widening the guard without replacing that algorithm would reintroduce corruption. For example, repeated floating-point division and subtraction can render an exactly representable integer such as `38802277692848472` incorrectly.

## Scope

This change will:

- render finite values accepted by the signed 64-bit contract through `%d`;
- allow `%f`, `%.2f`, and `%.0f` to render large integer-valued doubles below `2^63`;
- preserve truncation toward zero in `string.as-decimal`;
- preserve explicit failures for non-finite and out-of-range inputs;
- add regression coverage for the `2^53` boundary, representative large values, a value the old floating algorithm misrenders, and signed boundaries.

This change will not address the separate one-ULP rounding artifact reported for `printf "%.2f" 123456789012345.67`.

## Selected approach

`string.as-decimal` will remain the single decimal-integer rendering primitive used by `printf` and `string.as-fixed`. It will convert an accepted value to `i64` once, but it will not perform one expensive `i64` operation per decimal digit. Instead, it will use a hybrid base-`1_000_000_000` representation:

- magnitudes below the base use the original fast `number`-level digit loop after the signed-long conversion;
- larger magnitudes perform exactly one signed `i64.div` by the base, then render the quotient and remainder with that same fast loop;
- the remainder is left-padded to exactly nine decimal characters before it is appended to the quotient.

This split is exact because the absolute quotient is at most `9_223_372_036`, and the absolute remainder is at most `999_999_999`. Both values and every intermediate in their decimal digit loops are below `2^53` and therefore exactly representable as `number`.

The alternatives were rejected as follows:

- A `printf`-local formatter would duplicate `string.as-decimal` and require a second path through `string.as-fixed` for `%f`.
- Merely widening the existing guard to `2^63` would allow known floating-point digit corruption.
- Repeated signed `i64.div 10` is semantically plausible but too slow in pure EO. Even after quotient and remainder caching, five large `EOas_decimalTest` cases and one `EOprintfTest` case each hit the 20-minute JUnit timeout. The other 64 focused tests passed, isolating the problem to the intrinsic cost of per-digit `i64` division rather than range or formatting semantics.

## Design

### Conversion and range handling

`string.as-decimal` will first reject non-finite input as it does today. It will then enforce the signed 64-bit bounds before converting the truncated value with `number.as-i64`. The range check must preserve the existing `%d` contract: positive `2^63` is rejected, values below it are accepted, and the negative lower boundary is handled without overflowing an absolute-value operation.

The conversion will not rely on the fallback message inside `number.as-i64`, because that message formats the rejected number through `%f` and would recurse back into this formatting path.

### Exact digit extraction

After the range guards, `n.as-i64` is dataized and cached once as `integer!`. Every typed use redecorates those cached bytes as `i64 integer`; the implementation must not accidentally invoke `n.as-i64` again.

The internal digit routine works as follows:

1. Determine the sign from the truncated `i64` value. A minus sign is emitted only when `i64 integer` is negative, so a negative fraction that truncates to zero remains `0`.
2. When the accepted magnitude is below `1_000_000_000`, convert the safe truncated integer to `number`, take its `number`-level absolute value, and render it with the original division-by-ten digit loop.
3. Otherwise, divide `i64 integer` exactly once by `1_000_000_000.as-i64` and cache the quotient bytes.
4. Compute the signed remainder exactly as `integer - quotient * 1_000_000_000`, caching and redecorating typed intermediates. An established two's-complement `not`-plus-one subtraction pattern may be used instead of `i64.minus` to avoid its multiply-by-minus-one implementation.
5. Convert only the quotient and remainder to `number`, take their `number`-level absolute values, and render each through the fast small-number digit loop.
6. Left-pad the rendered remainder to exactly nine characters with zeroes and concatenate `head + tail`. Prefix the sign once around the completed unsigned string.

For example, `9223372036854774784` splits into `9223372036 | 854774784`, `38802277692848472` into `38802277 | 692848472`, `10000000000000000` into `10000000 | 000000000`, and `9007199254740992` into `9007199 | 254740992`. The signed minimum splits into `-9223372036 | -854775808`; the chunks are rendered by magnitude and the result receives one leading minus sign.

The algorithm deliberately never negates the complete input, so `-9223372036854775808` remains representable throughout.

Fractional inputs continue to truncate toward zero because `number.as-i64` already defines that behavior.

### Callers

No new formatting branch will be added to `string.printf`:

- `%d` continues to apply its long-range guard and delegates to `string.as-decimal`.
- `%f` continues through `string.as-fixed`; its integer part reaches the corrected `string.as-decimal` implementation.

This keeps width, zero padding, precision parsing, and failure propagation unchanged.

## Error behavior

- `nan`, positive infinity, and negative infinity terminate with the existing non-finite decimal error.
- Positive values at or above `2^63` and values below `-2^63` terminate before `number.as-i64` is invoked.
- `%d` retains its existing long-range diagnostic.
- `%f` continues to propagate the decimal conversion failure through `string.as-fixed` when its integer part is outside the supported range.

No fallback will silently round an out-of-range value or emit a partial string.

## Tests

Tests will be added before the implementation and observed failing for the current `2^53` guard.

`string.as-decimal` coverage will include:

- `2^53` and `1e16` rendering successfully;
- an exactly representable value that the old floating-point digit loop renders incorrectly;
- a value near the positive `2^63` boundary;
- the signed lower boundary without whole-value negation overflow;
- continued truncation of positive and negative fractions;
- rejection of non-finite and out-of-range values.

`string.printf` coverage will include the issue examples:

- `%d` with `9007199254740992` and `1.0e16`;
- `%f` with `9007199254740992`;
- `%.2f` and `%.0f` with `1.0e16`.

Performance verification is an acceptance gate, not only a correctness check. After generated test names are inspected, the signed-minimum regression will run alone and its Surefire testcase duration must be below five minutes. The largest positive and signed-minimum regressions will then run together to expose contention; both must pass without approaching the 20-minute JUnit limit. Only after those gates pass will the exact focused `EOas_decimalTest` and `EOprintfTest` command run. It must report `21` and `49` tests respectively, with zero failures and zero errors.

The rejected per-digit experiment established the baseline for this gate: `EOas_decimalTest` reported `21` tests with `5` errors, and `EOprintfTest` reported `49` tests with `1` error; all six errors were 20-minute timeouts. The other 64 focused tests passed. The timeout will not be weakened to accommodate the implementation.

## Acceptance criteria

- All issue examples below `2^63` produce the expected decimal strings.
- The known old-algorithm counterexample renders every digit correctly.
- Existing small-number formatting, truncation, padding, and precision behavior remains unchanged.
- Chunk tails are padded to exactly nine digits, including an all-zero tail.
- The signed minimum renders correctly without whole-value negation.
- Non-finite and out-of-range cases still fail deliberately.
- The signed-minimum testcase completes in less than five minutes, and the paired positive/negative boundary run completes comfortably below the existing 20-minute per-test timeout.
- The focused suite reports `EOas_decimalTest` as `21/0F/0E` and `EOprintfTest` as `49/0F/0E` without weakening timeouts.
