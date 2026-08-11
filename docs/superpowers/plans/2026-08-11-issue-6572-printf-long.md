# Issue #6572 Long-Range Decimal Formatting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `string.printf` render finite signed-long `number` values above `2^53` correctly for `%d` and `%f`, while preserving truncation and deliberate failures.

**Architecture:** Keep `string.as-decimal` as the shared integer-rendering path for `%d` and `string.as-fixed`. Convert each accepted `number` once to `i64`. Render safe values below a once-bound base of `1_000_000_000` with the original fast `number` digit loop. For larger values, estimate the bounded quotient with `number` arithmetic, validate and correct it at most one unit with exact `i64` multiplication and addition, render the safe exact chunks, and left-pad the remainder to nine digits. Never negate the full integer. `string.as-fixed` renders finite negative doubles at or above `2^53` directly as signed integral values and appends zero fraction digits, preserving the existing rounding path below that threshold.

**Tech Stack:** EO standard-library objects, embedded EO test declarations, Maven Surefire, Java 21.

---

## File map

- Modify `eo-runtime/src/main/eo/string/as-decimal.eo`: replace floating-point full-value digit extraction and the `2^53` guard with signed-long validation plus an estimated and exactly corrected base-`1_000_000_000` split; extend its embedded regression tests.
- Modify `eo-runtime/src/main/eo/string/printf.eo`: add issue-level `%d` and `%f` regression tests only; production routing remains unchanged.
- Modify `eo-runtime/src/main/eo/string/as-fixed.eo`: avoid negating finite negative integral doubles whose magnitude is at least `2^53`; keep ordinary rounding unchanged.

### Task 1: Add failing regressions for the documented range

**Files:**
- Modify: `eo-runtime/src/main/eo/string/as-decimal.eo:45-101`
- Modify: `eo-runtime/src/main/eo/string/printf.eo:370-611`

- [ ] **Step 1: Replace the obsolete `2^53` stopping test and add exact decimal boundary regressions**

Keep the existing small-number, fraction, zero, and `9007199254740991` tests. Replace `stops-on-a-magnitude-of-two-to-the-53rd` and add these declarations beside the other `as-decimal` tests:

```eo
  eq. ++> can-write-two-to-the-53rd
    "9007199254740992"
    string
      as-decimal 9007199254740992

  eq. ++> can-write-ten-to-the-16th
    "10000000000000000"
    string
      as-decimal 1.0e16

  eq. ++> can-write-a-large-exact-counterexample
    "38802277692848472"
    string
      as-decimal 38802277692848472

  eq. ++> can-write-the-largest-double-below-the-long-maximum
    "9223372036854774784"
    string
      as-decimal 9223372036854774784

  eq. ++> can-write-the-long-minimum
    "-9223372036854775808"
    string
      as-decimal -9.223372036854776e18

  as-decimal 9.223372036854776e18 --> stops-on-the-long-maximum-boundary

  as-decimal -9.223372036854778e18 --> stops-below-the-long-minimum-boundary
```

- [ ] **Step 2: Add the five issue examples to `printf`**

Place these declarations with the other successful formatting tests:

```eo
  eq. ++> can-format-two-to-the-53rd-as-an-integer
    "9007199254740992"
    printf *1
      "%d"
      9007199254740992

  eq. ++> can-format-ten-to-the-16th-as-an-integer
    "10000000000000000"
    printf *1
      "%d"
      1.0e16

  eq. ++> can-format-two-to-the-53rd-as-a-float
    "9007199254740992.000000"
    printf *1
      "%f"
      9007199254740992

  eq. ++> can-format-ten-to-the-16th-with-two-fraction-digits
    "10000000000000000.00"
    printf *1
      "%.2f"
      1.0e16

  eq. ++> can-format-ten-to-the-16th-without-fraction-digits
    "10000000000000000"
    printf *1
      "%.0f"
      1.0e16
```

- [ ] **Step 3: Run only the generated EO test classes and confirm the red state**

Run in PowerShell:

```powershell
$env:JAVA_HOME='E:\tools\ProfessionalTools\jdk21\jdk\jdk-21.0.10+7'
$env:Path="$env:JAVA_HOME\bin;$env:Path"
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest,org.eolang.EO_string.EOprintfTest' test -pl :eo-runtime
```

Expected: the command fails because the new successful cases bottom out at the current `2^53` guard. The reported cause contains `its magnitude is 2^53 or larger`; pre-existing small and failure cases remain successful.

- [ ] **Step 4: Commit the red tests**

```powershell
git add -- eo-runtime/src/main/eo/string/as-decimal.eo eo-runtime/src/main/eo/string/printf.eo
git diff --cached --check
git commit -m '#6572: reproduce long-range printf failures'
```

Expected: one commit containing only the two EO test files.

### Task 2: Render signed-long decimals with an exact corrected base-`1_000_000_000` split

**Files:**
- Modify: `eo-runtime/src/main/eo/string/as-decimal.eo:11-42`
- Test: `eo-runtime/src/main/eo/string/as-decimal.eo:45-115`
- Test: `eo-runtime/src/main/eo/string/printf.eo:370-640`

- [ ] **Step 1: Replace the conversion and digit routine in `as-decimal`**

Leave the file header and all Task 1 embedded test declarations intact. Replace only the production body with this exact algorithm:

1. Reject non-finite input, `n >= 2^63`, and `n < -2^63`, in that order, before invoking `number.as-i64`.
2. Dataize `n.as-i64` exactly once as `integer!`; redecorate every typed use as `i64 integer` because a `!` binding retains bytes rather than its decoration.
3. Determine whether to prefix `-` from the truncated `i64`, not from `n`. This keeps negative fractions that truncate to zero unsigned.
4. Define `small-digits` by retaining the original fast `number`-level division, floor, remainder, and ASCII conversion loop. Call it only with exact nonnegative integers below `2^53`.
5. Bind numeric base `1_000_000_000` once. If `n.abs < base`, render the absolute value of `(i64 integer).as-number` directly through `small-digits`.
6. Otherwise compute `floor(n.abs / base)`, apply the sign using safe `number` arithmetic, convert the bounded estimate to `i64`, and cache its bytes. Do not use `i64.div` and do not negate the full integer.
7. Cache the exact `i64` product `estimate * base`, then cache the signed remainder as `integer - product` with two's-complement `not`-plus-one addition.
8. Correct at most one unit using exact signed-remainder invariants. Positive inputs require `0 <= remainder < base`: decrement/add-base for a negative remainder or increment/subtract-base for a remainder at least base. Negative inputs require `-base < remainder <= 0`: increment/subtract-base for a positive remainder or decrement/add-base for a remainder at or below negative base. Cache the corrected quotient and remainder bytes.
9. Convert only the corrected quotient and remainder to `number`, take their `number`-level absolute values, and render them with `small-digits`. The quotient magnitude is at most `9_223_372_036`; the remainder magnitude is at most `999_999_999`, so both paths are exact. An overestimate near the maximum quotient cannot reach `9_223_372_037`, because the `2^63` magnitude boundary is still `145_224_192` below that base multiple, while the estimate error is sub-micro-unit.
10. Left-pad the tail string to exactly nine characters with zeroes and concatenate the unsigned head and tail. Prefix one minus sign only when the truncated `i64` is negative.

The chunk boundary behavior must include `10000000000000000 -> 10000000 | 000000000`, `9007199254740992 -> 9007199 | 254740992`, `38802277692848472 -> 38802277 | 692848472`, `9223372036854774784 -> 9223372036 | 854774784`, and `-9223372036854775808 -> -9223372036 | -854775808` before magnitude rendering and the single sign prefix.

Do not revive either rejected division algorithm. Per-decimal-digit division produced `EOas_decimalTest 21/0F/5E` and `EOprintfTest 49/0F/1E`; all six errors were 20-minute timeouts while the other 64 focused tests passed. The later one-division base split was correct but still required 58–88 seconds for isolated large values and hundreds of seconds for individual contended suite cases, which is not acceptable for production formatting.

- [ ] **Step 2: Inspect the implementation diff for accidental caller changes**

Run:

```powershell
git diff --check
git diff -- eo-runtime/src/main/eo/string/as-decimal.eo eo-runtime/src/main/eo/string/printf.eo eo-runtime/src/main/eo/string/as-fixed.eo
```

Expected: production changes appear only in `as-decimal.eo` and `as-fixed.eo`; `printf.eo` contains test additions only.

Do not run the complete focused classes or commit the production source yet. Task 3 applies the performance gates in increasing scope before staging the implementation.

### Task 3: Enforce the performance gate, verify the branch, and prepare it for review

**Files:**
- Verify: `eo-runtime/src/main/eo/string/as-decimal.eo`
- Verify: `eo-runtime/src/main/eo/string/printf.eo`
- Verify: `docs/superpowers/specs/2026-08-11-issue-6572-printf-long-design.md`
- Verify: `docs/superpowers/plans/2026-08-11-issue-6572-printf-long.md`

- [ ] **Step 1: Inspect the generated test name and time the signed-minimum test alone**

Inspect `eo-runtime/target/generated-test-sources/org/eolang/EO_string/EOas_decimalTest.java` and confirm that `can-write-the-long-minimum` generated the Java method `can_write_the_long_minimum`. Then run only that method with Java 21:

```powershell
$env:JAVA_HOME='E:\tools\ProfessionalTools\jdk21\jdk\jdk-21.0.10+7'
$env:Path="$env:JAVA_HOME\bin;$env:Path"
$watch=[System.Diagnostics.Stopwatch]::StartNew()
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest#can_write_the_long_minimum' test -pl :eo-runtime
$code=$LASTEXITCODE
$watch.Stop()
"Maven wall time: {0:N3}s" -f $watch.Elapsed.TotalSeconds
exit $code
```

Record both the Maven wall time and the testcase duration from `eo-runtime/target/surefire-reports/TEST-org.eolang.EO_string.EOas_decimalTest.xml`.

Expected: the test passes and its Surefire testcase duration is less than ten seconds, preferably substantially lower. If it reaches ten seconds, stop before any broader run and redesign the implementation; do not weaken the existing timeout.

- [ ] **Step 2: Run the largest positive and signed-minimum methods together**

After confirming that the generated method names are `can_write_the_largest_double_below_the_long_maximum` and `can_write_the_long_minimum`, run:

```powershell
$watch=[System.Diagnostics.Stopwatch]::StartNew()
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest#can_write_the_largest_double_below_the_long_maximum+can_write_the_long_minimum' test -pl :eo-runtime
$code=$LASTEXITCODE
$watch.Stop()
"Maven wall time: {0:N3}s" -f $watch.Elapsed.TotalSeconds
exit $code
```

Expected: both pass under contention, and neither approaches the 20-minute JUnit timeout.

- [ ] **Step 3: Run the exact focused EO suite**

Run:

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest,org.eolang.EO_string.EOas_fixedTest,org.eolang.EO_string.EOprintfTest' test -pl :eo-runtime
```

Expected: all three focused classes report zero failures and zero errors. Record each class count and duration from the Surefire reports. The rejected per-digit baseline was `EOas_decimalTest 21/0F/5E` and `EOprintfTest 49/0F/1E`, with all six errors caused by 20-minute timeouts.

- [ ] **Step 4: Stage and commit only the verified formatter implementation**

Run:

```powershell
git diff --check
git diff --name-only
git diff -- eo-runtime/src/main/eo/string/as-decimal.eo eo-runtime/src/main/eo/string/printf.eo eo-runtime/src/main/eo/string/as-fixed.eo
git add -- eo-runtime/src/main/eo/string/as-decimal.eo eo-runtime/src/main/eo/string/as-fixed.eo
git diff --cached --name-only
git diff --cached --check
git commit -m '#6572: avoid long division in decimal formatting'
```

Expected: before staging, the only uncommitted production files are `as-decimal.eo` and `as-fixed.eo`, with `printf.eo` containing tests only. The production commit contains only `as-decimal.eo` and `as-fixed.eo`; the regressions are already committed separately.

- [ ] **Step 5: Run repository hygiene checks and inspect the complete branch diff**

Run:

```powershell
git diff upstream/master...HEAD --check
git status --short
git diff --stat upstream/master...HEAD
git log --oneline --decorate upstream/master..HEAD
```

Expected: no whitespace errors, no uncommitted source changes, and only the design, plan, regression tests, and formatter implementation are present.

- [ ] **Step 6: Request a code review and address only verified findings**

Use `superpowers:requesting-code-review` against `upstream/master...HEAD`. Check the review against the issue scope, rerun the focused tests after any correction, and create a narrowly scoped follow-up commit only if a concrete defect is found.

### Review follow-up: reject pure-EO division latency and cover the signed minimum

- [ ] Amend this plan and the design before source changes. Record the one-division latency, the number estimate plus exact `i64` correction, the ten-second isolated gate, the base seams, and the `as-fixed` signed-minimum path. Commit docs only as `#6572: refine decimal formatting performance design`.
- [ ] Add positive and negative `as-decimal` regressions for `999999999`, `1000000000`, `1000000001`, and `1000000042`, plus a large fractional truncation case. Add `printf` regressions for `%d`, `%f`, and `%.0f` at `-2^63`. Run the three focused classes before production edits and verify that only the `%f` and `%.0f` signed-minimum cases fail. Commit tests only as `#6572: cover decimal chunk boundaries`.
- [ ] Replace the general `i64.div` split with the bounded estimate and exact one-unit correction. Add the narrow finite-negative-large branch to `as-fixed`; retain its rounding path for smaller magnitudes.
- [ ] Run the signed-minimum `as-decimal` method alone and require its testcase duration below ten seconds. Then run seam methods, the largest-positive/signed-minimum pair, and the exact three focused classes. Inspect counts and timings, production diffs, branch cleanliness, and owned processes before committing production only as `#6572: avoid long division in decimal formatting`.

- [ ] **Step 7: Push and open the requested Draft PR**

Run the publishing workflow from `github:yeet`: confirm GitHub authentication, push `agent/issue-6572-printf-long` to `origin`, and create a Draft PR against `objectionary/eo:master`. The PR body must explain the old `2^53` guard, the exact `i64` extraction, the test evidence, the known unrelated Windows baseline failure if it recurs, and include `Fixes #6572`.
