# Issue #6572 Long-Range Decimal Formatting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `string.printf` render finite signed-long `number` values above `2^53` correctly for `%d` and `%f`, while preserving truncation and deliberate failures.

**Architecture:** Keep `string.as-decimal` as the shared integer-rendering path for `%d` and `string.as-fixed`. Convert each accepted `number` once to `i64`, extract digits with exact signed `i64` quotient/remainder operations, and never negate the full input so the signed minimum remains representable.

**Tech Stack:** EO standard-library objects, embedded EO test declarations, Maven Surefire, Java 21.

---

## File map

- Modify `eo-runtime/src/main/eo/string/as-decimal.eo`: replace floating-point digit extraction and the `2^53` guard with signed-long validation plus exact `i64` digit extraction; extend its embedded regression tests.
- Modify `eo-runtime/src/main/eo/string/printf.eo`: add issue-level `%d` and `%f` regression tests only; production routing remains unchanged.
- Reference `eo-runtime/src/main/eo/string/as-fixed.eo`: verify that `%f` still reaches `string.as-decimal` through its integer part; no source change is planned.

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

### Task 2: Render decimal digits with exact signed `i64` arithmetic

**Files:**
- Modify: `eo-runtime/src/main/eo/string/as-decimal.eo:11-42`
- Test: `eo-runtime/src/main/eo/string/as-decimal.eo:45-115`
- Test: `eo-runtime/src/main/eo/string/printf.eo:370-640`

- [ ] **Step 1: Replace the conversion and digit routine in `as-decimal`**

Replace the production body above the embedded tests with this implementation, leaving the file header and test declarations intact:

```eo
[n] > as-decimal
  n.is-finite.not.if > @
    T
      "Can't write a non-finite number as decimal"
    if.
      or.
        n.gte 9.223372036854776e18
        n.lt -9.223372036854776e18
      T
        "Can't write this number as decimal digits: it is outside the signed 64-bit range"
      if.
        integer.lt 0.as-i64
        "-".as-bytes.concat
          digits integer
        digits integer
  [n] >> digit
    as-char
      plus.
        if.
          n.lt 0.as-i64
          n.neg.as-number
          n.as-number
        48
  [n] >> digits
    if. > @
      q.eq 0.as-i64
      digit remainder
      concat.
        digits q
        digit remainder
    n.div 10.as-i64 > q
    n.minus (q.times 10.as-i64) > remainder
  n.as-i64 > integer!
```

The positive upper comparison is inclusive because the double literal is exactly `2^63`; the negative comparison is strict so exactly `-2^63` remains accepted. `digit` negates only a remainder from `-9` through `-1`, never the whole long minimum.

- [ ] **Step 2: Run the two focused EO test classes and confirm the green state**

Run:

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest,org.eolang.EO_string.EOprintfTest' test -pl :eo-runtime
```

Expected: `EOas_decimalTest` and `EOprintfTest` pass with zero failures and zero errors, including the existing truncation, padding, precision, non-finite, and out-of-range cases.

- [ ] **Step 3: Inspect the implementation diff for accidental caller changes**

Run:

```powershell
git diff --check
git diff -- eo-runtime/src/main/eo/string/as-decimal.eo eo-runtime/src/main/eo/string/printf.eo eo-runtime/src/main/eo/string/as-fixed.eo
```

Expected: production changes appear only in `as-decimal.eo`; `printf.eo` contains test additions only; `as-fixed.eo` has no diff.

- [ ] **Step 4: Commit the exact formatter implementation**

```powershell
git add -- eo-runtime/src/main/eo/string/as-decimal.eo
git diff --cached --check
git commit -m '#6572: format signed long numbers exactly'
```

Expected: one commit containing the production change in `as-decimal.eo`; the `printf.eo` regression tests are already committed in Task 1.

### Task 3: Verify the branch and prepare it for review

**Files:**
- Verify: `eo-runtime/src/main/eo/string/as-decimal.eo`
- Verify: `eo-runtime/src/main/eo/string/printf.eo`
- Verify: `docs/superpowers/specs/2026-08-11-issue-6572-printf-long-design.md`
- Verify: `docs/superpowers/plans/2026-08-11-issue-6572-printf-long.md`

- [ ] **Step 1: Rebuild the focused tests from clean generated sources**

Run:

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest,org.eolang.EO_string.EOprintfTest' clean test -pl :eo-runtime
```

Expected: both generated EO test classes pass from a clean build with zero failures and zero errors.

- [ ] **Step 2: Run the full `eo-runtime` unit-test suite**

Run:

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' test -pl :eo-runtime
```

Expected on this Windows host: no regression attributable to this branch. The previously reproduced environment-specific `SyscallTest.WindowsSocketTest.refusesConnectionViaSyscall` may remain the sole failure because `192.0.2.1` accepts the connection here; record its exact counts and output separately if it recurs.

- [ ] **Step 3: Run repository hygiene checks and inspect the complete branch diff**

Run:

```powershell
git diff upstream/master...HEAD --check
git status --short
git diff --stat upstream/master...HEAD
git log --oneline --decorate upstream/master..HEAD
```

Expected: no whitespace errors, no uncommitted source changes, and only the design, plan, regression tests, and formatter implementation are present.

- [ ] **Step 4: Request a code review and address only verified findings**

Use `superpowers:requesting-code-review` against `upstream/master...HEAD`. Check the review against the issue scope, rerun the focused tests after any correction, and create a narrowly scoped follow-up commit only if a concrete defect is found.

- [ ] **Step 5: Push and open the requested Draft PR**

Run the publishing workflow from `github:yeet`: confirm GitHub authentication, push `agent/issue-6572-printf-long` to `origin`, and create a Draft PR against `objectionary/eo:master`. The PR body must explain the old `2^53` guard, the exact `i64` extraction, the test evidence, the known unrelated Windows baseline failure if it recurs, and include `Fixes #6572`.
