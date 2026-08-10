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

`string.as-decimal` will remain the single decimal-integer rendering primitive used by `printf` and `string.as-fixed`, but its digit extraction will operate on `i64` values instead of `number` arithmetic.

The alternatives were rejected as follows:

- A `printf`-local formatter would duplicate `string.as-decimal` and require a second path through `string.as-fixed` for `%f`.
- Merely widening the existing guard to `2^63` would allow known floating-point digit corruption.

## Design

### Conversion and range handling

`string.as-decimal` will first reject non-finite input as it does today. It will then enforce the signed 64-bit bounds before converting the truncated value with `number.as-i64`. The range check must preserve the existing `%d` contract: positive `2^63` is rejected, values below it are accepted, and the negative lower boundary is handled without overflowing an absolute-value operation.

The conversion will not rely on the fallback message inside `number.as-i64`, because that message formats the rejected number through `%f` and would recurse back into this formatting path.

### Exact digit extraction

The internal digit routine will use exact `i64` operations:

1. Divide the current signed integer by `10.as-i64`; `i64.div` truncates toward zero.
2. Compute the signed remainder as `current - quotient * 10`.
3. Negate only the single-digit remainder when it is negative, then convert that value to `number` and add the ASCII offset.
4. If the quotient is zero, return that digit; otherwise recurse on the quotient and append the digit, producing most-significant-first order.

The algorithm deliberately does not negate the complete input. A minus sign is prefixed only when the original input is negative and its truncated `i64` value is nonzero. This allows the signed minimum value to render without overflowing `i64`, while a negative fraction that truncates to zero remains `0`.

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

Verification will use targeted `eo-runtime` tests first, followed by the module-level build. The clean `master` baseline has one unrelated Windows environment failure in `SyscallTest`: this machine accepts a connection to `192.0.2.1` where the test expects refusal. That failure will be recorded separately from any regression introduced by this change.

## Acceptance criteria

- All issue examples below `2^63` produce the expected decimal strings.
- The known old-algorithm counterexample renders every digit correctly.
- Existing small-number formatting, truncation, padding, and precision behavior remains unchanged.
- Non-finite and out-of-range cases still fail deliberately.
- Targeted tests pass with no failures or errors.
- Module verification introduces no failures beyond the documented `SyscallTest` baseline failure.
