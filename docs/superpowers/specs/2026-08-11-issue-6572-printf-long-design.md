# Issue #6572: long-range decimal formatting

## Context

[`string.printf`](https://github.com/objectionary/eo/issues/6572) documents `%d` around the signed 64-bit range, but both `%d` and `%f` delegate integer rendering to `string.as-decimal`. That object currently rejects every magnitude at or above `2^53`, so exactly representable `number` values between `2^53` and `2^63` fail before formatting.

The `2^53` guard was introduced to prevent the existing floating-point digit extraction from emitting incorrect or non-digit bytes. Removing or widening the guard without replacing that algorithm would reintroduce corruption. For example, repeated floating-point division and subtraction can render an exactly representable integer such as `38802277692848472` incorrectly.

The first exact signed-long implementation split large integers with one pure-EO `i64.div` by `1_000_000_000`. It produced correct digits, but isolated large cases took 58–88 seconds and contended suite cases took hundreds of seconds. It also exposed a caller defect: `string.as-fixed` negated every negative input before rendering, so the signed minimum became the out-of-range positive value `2^63` for `%f` and `%.0f`.

## Scope

This change will:

- render finite values accepted by the signed 64-bit contract through `%d`;
- allow `%f`, `%.2f`, and `%.0f` to render large integer-valued doubles below `2^63`;
- preserve truncation toward zero in `string.as-decimal`;
- preserve explicit failures for non-finite and out-of-range inputs;
- render the signed minimum through `%f` without negating the whole value;
- add regression coverage for the `2^53` boundary, representative large values, a value the old floating algorithm misrenders, and signed boundaries.

This change will not address the separate one-ULP rounding artifact reported for `printf "%.2f" 123456789012345.67`.

## Selected approach

`string.as-decimal` will remain the single decimal-integer rendering primitive used by `printf` and `string.as-fixed`. It will convert an accepted value to `i64` once, but it will not perform one expensive `i64` operation per decimal digit. Instead, it will use a hybrid base-`1_000_000_000` representation:

- magnitudes below the base use the original fast `number`-level digit loop after the signed-long conversion;
- larger magnitudes estimate the quotient with bounded `number` arithmetic, convert that small signed estimate to `i64`, and validate and, when necessary, correct it with exact `i64` multiplication and addition;
- the remainder is left-padded to exactly nine decimal characters before it is appended to the quotient.

The numeric base is bound once so the estimate, product, correction, threshold, and padding cannot drift to different literals. The estimated quotient magnitude is `floor(abs(n) / base)`. It is at most `9_223_372_036`, so both it and its signed form are exact `number` values and safe to convert to `i64`. IEEE-754 division at a quotient magnitude near `10^10` has a sub-micro-unit ULP, while exact quotient boundaries are one unit apart and the input grid contributes steps of `10^-9` quotient units. The estimate can therefore cross at most one boundary. Exact signed-remainder checks correct that possible one-unit error.

After correction, positive inputs satisfy `0 <= remainder < base`; negative inputs satisfy `-base < remainder <= 0`. The absolute corrected quotient is at most `9_223_372_036`, and the absolute corrected remainder is at most `999_999_999`. Both chunks and every intermediate in their decimal digit loops are below `2^53` and therefore exactly representable as `number`. The exact `i64` product is also in range: any one-unit overestimate can occur only next to an in-range base multiple, while the `2^63` magnitude boundary is still `145_224_192` integers below the next multiple, far beyond the floating estimate error, so the estimate cannot become `9_223_372_037`.

The alternatives were rejected as follows:

- A `printf`-local formatter would duplicate `string.as-decimal` and require a second path through `string.as-fixed` for `%f`.
- Merely widening the existing guard to `2^63` would allow known floating-point digit corruption.
- Repeated signed `i64.div 10` is semantically plausible but too slow in pure EO. Even after quotient and remainder caching, five large `EOas_decimalTest` cases and one `EOprintfTest` case each hit the 20-minute JUnit timeout. The other 64 focused tests passed, isolating the problem to the intrinsic cost of per-digit `i64` division rather than range or formatting semantics.
- One signed pure-EO `i64.div 1_000_000_000` per large value is functionally correct but still production-unacceptable: isolated large cases require 58–88 seconds, and individual cases take hundreds of seconds under suite contention. Correctness alone does not satisfy the formatter's latency requirement.

## Design

### Conversion and range handling

`string.as-decimal` will first reject non-finite input as it does today. It will then enforce the signed 64-bit bounds before converting the truncated value with `number.as-i64`. The range check must preserve the existing `%d` contract: positive `2^63` is rejected, values below it are accepted, and the negative lower boundary is handled without overflowing an absolute-value operation.

The conversion will not rely on the fallback message inside `number.as-i64`, because that message formats the rejected number through `%f` and would recurse back into this formatting path.

### Exact digit extraction

After the range guards, `n.as-i64` is dataized and cached once as `integer!`. Every typed use redecorates those cached bytes as `i64 integer`; the implementation must not accidentally invoke `n.as-i64` again.

The internal digit routine works as follows:

1. Determine the sign from the truncated `i64` value. A minus sign is emitted only when `i64 integer` is negative, so a negative fraction that truncates to zero remains `0`.
2. Bind the numeric base once. When the accepted magnitude is below the base, convert the safe truncated integer to `number`, take its `number`-level absolute value, and render it with the original division-by-ten digit loop.
3. Otherwise, compute `floor(n.abs / base)`, apply the sign with safe `number` arithmetic, convert that bounded value to `i64`, and cache the estimated quotient bytes. No full-width integer is negated.
4. Compute and cache `estimated quotient * base` exactly in `i64`, then compute the signed remainder exactly as `integer - product` with two's-complement `not`-plus-one addition.
5. Correct at most one quotient unit from the exact remainder. For a positive integer, a negative remainder decrements the quotient and adds the base; a remainder at least the base increments the quotient and subtracts the base. For a negative integer, a positive remainder increments the quotient and subtracts the base; a remainder at or below negative base decrements the quotient and adds the base. Cache the corrected quotient and remainder bytes.
6. Convert only the corrected quotient and remainder to `number`, take their `number`-level absolute values, and render each through the fast small-number digit loop.
7. Left-pad the rendered remainder to exactly nine characters with zeroes and concatenate `head + tail`. Prefix the sign once around the completed unsigned string.

For example, `9223372036854774784` splits into `9223372036 | 854774784`, `38802277692848472` into `38802277 | 692848472`, `10000000000000000` into `10000000 | 000000000`, and `9007199254740992` into `9007199 | 254740992`. The signed minimum splits into `-9223372036 | -854775808`; the chunks are rendered by magnitude and the result receives one leading minus sign.

The algorithm deliberately never negates the complete input, so `-9223372036854775808` remains representable throughout.

Fractional inputs continue to truncate toward zero because `number.as-i64` already defines that behavior.

### Callers

No new formatting branch will be added to `string.printf`:

- `%d` continues to apply its long-range guard and delegates to `string.as-decimal`.
- `%f` continues through `string.as-fixed`. After validating `places`, a finite negative value with magnitude at least `2^53` is already integral as a double, so `as-fixed` renders that signed integer directly through `string.as-decimal` and appends a decimal point plus exactly `places` zeroes when `places > 0`. A zero precision returns only the signed integer. Smaller magnitudes retain the existing rounding path.

This keeps width, zero padding, precision parsing, and failure propagation unchanged.

## Error behavior

- `nan`, positive infinity, and negative infinity terminate with the existing non-finite decimal error.
- Positive values at or above `2^63` and values below `-2^63` terminate before `number.as-i64` is invoked.
- `%d` retains its existing long-range diagnostic.
- `%f` continues to propagate the decimal conversion failure through `string.as-fixed` when its integer part is outside the supported range.
- `nan` and infinities still reach deliberate non-finite handling; the signed-large shortcut is restricted to finite values.

No fallback will silently round an out-of-range value or emit a partial string.

## Tests

Tests will be added before the implementation and observed failing for the current `2^53` guard.

`string.as-decimal` coverage will include:

- `2^53` and `1e16` rendering successfully;
- an exactly representable value that the old floating-point digit loop renders incorrectly;
- a value near the positive `2^63` boundary;
- the signed lower boundary without whole-value negation overflow;
- continued truncation of positive and negative fractions;
- rejection of non-finite and out-of-range values;
- positive and negative seams at `999_999_999`, `1_000_000_000`, `1_000_000_001`, and `1_000_000_042`;
- fractional truncation on the large path.

`string.printf` coverage will include the issue examples:

- `%d` with `9007199254740992` and `1.0e16`;
- `%f` with `9007199254740992`;
- `%.2f` and `%.0f` with `1.0e16`;
- `%d`, `%f`, and `%.0f` with `-2^63`.

Performance verification is an acceptance gate, not only a correctness check. After generated test names are inspected, the signed-minimum regression will run alone and its Surefire testcase duration must be below ten seconds, with a substantially lower result preferred. The seam regressions and the largest-positive/signed-minimum pair will then run before the exact focused `EOas_decimalTest`, `EOas_fixedTest`, and `EOprintfTest` classes. The timeout will not be weakened to accommodate the implementation.

The rejected per-digit experiment established the baseline for this gate: `EOas_decimalTest` reported `21` tests with `5` errors, and `EOprintfTest` reported `49` tests with `1` error; all six errors were 20-minute timeouts. The other 64 focused tests passed. The timeout will not be weakened to accommodate the implementation.

## Acceptance criteria

- All issue examples below `2^63` produce the expected decimal strings.
- The known old-algorithm counterexample renders every digit correctly.
- Existing small-number formatting, truncation, padding, and precision behavior remains unchanged.
- Chunk tails are padded to exactly nine digits, including an all-zero tail.
- The signed minimum renders correctly without whole-value negation.
- The quotient/remainder seams render correctly on both sides of zero, and a large fraction still truncates toward zero.
- Non-finite and out-of-range cases still fail deliberately.
- The signed-minimum testcase completes in less than ten seconds, and the seam and paired boundary runs pass without weakening timeouts.
- The focused `EOas_decimalTest`, `EOas_fixedTest`, and `EOprintfTest` classes pass with zero failures and zero errors.
