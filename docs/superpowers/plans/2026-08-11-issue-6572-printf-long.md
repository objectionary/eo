# Issue #6572 Native Signed-Long Decimal Formatting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `string.printf` render finite signed-long `number` values above `2^53` exactly and fast enough for production by delegating the already-validated `i64` bytes to one narrow JVM atom.

**Architecture:** Keep all non-finite/range checks and the sole `number.as-i64` conversion in `string.as-decimal`; pass the cached bytes, redecorated as `i64`, to a nested atom that decodes one Java `long` and returns its locale-independent decimal text as UTF-8 `Q.bytes`. Route every finite value whose magnitude is at least `2^53` through one `string.as-fixed` helper that renders the integral value directly and appends zero fraction digits, avoiding both positive-value double scaling drift and `Long.MIN_VALUE` negation overflow. Do not change production `string.printf`.

**Tech Stack:** EO standard-library objects, Java runtime atoms, JUnit 5, Hamcrest, Maven Surefire, Java 21.

---

## File map

- Modify `docs/superpowers/specs/2026-08-11-issue-6572-printf-long-design.md`: replace the rejected estimate/chunk design with the measured JVM-atom exception, exact API, performance gate, and Node parity debt.
- Modify `docs/superpowers/plans/2026-08-11-issue-6572-printf-long.md`: define the test-first implementation and validation sequence.
- Modify `eo-runtime/src/main/eo/string/as-decimal.eo`: retain the guards and sole `n.as-i64`, declare and call `from-i64 /Q.bytes`.
- Modify `eo-runtime/src/main/eo/string/as-fixed.eo`: render finite positive and negative integral doubles whose magnitude is at least `2^53` directly, avoiding positive double scaling drift and whole-value negation of `Long.MIN_VALUE`.
- Create `eo-runtime/src/main/java/org/eolang/EO_string/EOas_decimal$EOfrom_i64.java`: exact signed-long-to-UTF-8 atom.
- Create `eo-runtime/src/test/java/org/eolang/EO_string/EOas_decimalEOfrom_i64Test.java`: direct atom contract tests.
- Preserve `eo-runtime/src/main/eo/string/printf.eo`: its issue regressions are already committed; no production edit is permitted.

## Evidence and exception boundary

Pure EO has been exhausted for this path. Per-digit exact division produced six 20-minute timeouts while 64 other focused tests passed. A single base-`1_000_000_000` `i64.div` took 58–88 seconds in isolation. The fastest quotient/chunk variants still took 21.890–23.386 seconds, and the remaining variants took 27–88 seconds. Every result exceeds the required `<10s` isolated signed-minimum testcase gate.

The JVM atom is therefore a measured performance exception. Recent `dataized` and `recovered` changes establish Java-only atom precedent after the `eo2js` archive, and Maven/CI validate JVM atom metadata only. All current atom EO sources still declare both JVM and Node locators, so `as-decimal.eo` does too. This branch does not add a JavaScript counterpart: Node atom lookup remains unsupported and must be tracked in a separate Node-parity issue against the archived/runtime successor project. Do not claim Node compatibility and do not expand this branch into external `eo2js` work.

### Task 1: Commit the native design decision

**Files:**

- Modify: `docs/superpowers/specs/2026-08-11-issue-6572-printf-long-design.md`
- Modify: `docs/superpowers/plans/2026-08-11-issue-6572-printf-long.md`

- [ ] **Step 1: Replace pure-EO production experiments without losing committed tests**

Use `apply_patch` to reduce the uncommitted `as-decimal.eo` production body to the guarded cached conversion and nested declaration shown in Task 3. Retain every embedded EO test already committed in `b163d32d6`. Keep only the narrow finite-large `as-fixed` branch shown in Task 3. Leave these production files unstaged during the documentation commit.

- [ ] **Step 2: Record the final architecture and evidence**

The design and this plan must state the 20-minute per-digit timeouts, 58–88 second division result, 21.890–23.386 second best range, 27–88 second remaining range, exact EO/Java API, wrong-width `ExFailure`, UTF-8 output, `<10s` gate, `as-fixed` fix, JVM-only precedent, and separate Node parity debt.

- [ ] **Step 3: Check and commit documentation only**

Run:

```powershell
git diff --check -- docs/superpowers/specs/2026-08-11-issue-6572-printf-long-design.md docs/superpowers/plans/2026-08-11-issue-6572-printf-long.md
git add -- docs/superpowers/specs/2026-08-11-issue-6572-printf-long-design.md docs/superpowers/plans/2026-08-11-issue-6572-printf-long.md
git diff --cached --name-only
git diff --cached --check
git commit -m '#6572: adopt native decimal conversion design'
```

Expected: the staged file list contains exactly the design and plan; the production EO files remain uncommitted.

### Task 2: Specify and observe the direct JVM atom contract

**Files:**

- Create: `eo-runtime/src/test/java/org/eolang/EO_string/EOas_decimalEOfrom_i64Test.java`

- [ ] **Step 1: Add the direct test before the Java atom**

Create the test with `apply_patch`. The test class references the not-yet-present `EOas_decimal$EOfrom_i64` and contains this behavior:

```java
@ParameterizedTest
@CsvSource({
    "-9223372036854775808, -9223372036854775808",
    "9223372036854775807, 9223372036854775807",
    "0, 0",
    "42, 42",
    "-42, -42",
    "38802277692848472, 38802277692848472"
})
void convertsSignedLongToDecimalBytes(final long input, final String expected) {
    MatcherAssert.assertThat(
        "signed i64 bytes must become exact decimal UTF-8 bytes",
        new Dataized(EOas_decimalEOfrom_i64Test.application(input)).asString(),
        Matchers.equalTo(expected)
    );
}
```

Add a wrong-width case that passes one byte and asserts `ExFailure`, plus a result-forma case that invokes `take(Phi.LAMBDA)` and asserts `Φ.bytes`. Build applications with exact binary long bytes, never `Data.ToPhi(Long)`:

```java
private static Phi application(final long value) {
    return EOas_decimalEOfrom_i64Test.application(
        new Data.ToPhi(new BytesOf(value).take())
    );
}

private static Phi application(final Phi value) {
    return new PhApplication(
        new EOas_decimal$EOfrom_i64(), "value", value
    );
}
```

- [ ] **Step 2: Run the intended RED with JDK 21**

Run one owned Maven process:

```powershell
$Issue6572Jdk='E:\tools\ProfessionalTools\jdk21\jdk\jdk-21.0.10+7'
$env:JAVA_HOME=$Issue6572Jdk
$env:Path="$Issue6572Jdk\bin;$env:Path"
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalEOfrom_i64Test' test -pl :eo-runtime
```

Expected: test compilation fails only because `EOas_decimal$EOfrom_i64` is absent. Record the compiler diagnostic; do not create production Java before observing this RED.

- [ ] **Step 3: Commit the test alone**

Run:

```powershell
git add -- eo-runtime/src/test/java/org/eolang/EO_string/EOas_decimalEOfrom_i64Test.java
git diff --cached --name-only
git diff --cached --check
git commit -m '#6572: cover native signed decimal conversion'
```

Expected: the commit contains exactly the direct Java test.

### Task 3: Implement the minimal native conversion and signed-large caller fix

**Files:**

- Modify: `eo-runtime/src/main/eo/string/as-decimal.eo`
- Modify: `eo-runtime/src/main/eo/string/as-fixed.eo`
- Create: `eo-runtime/src/main/java/org/eolang/EO_string/EOas_decimal$EOfrom_i64.java`

- [ ] **Step 1: Keep the exact EO seam**

Add the standard runtime locators and use this production shape before the existing embedded tests:

```eo
+rt jvm org.eolang:eo-runtime:0.0.0
+rt node eo2js-runtime:0.0.0

[n] > as-decimal
  n.is-finite.not.if > @
    T
      "Can't write a non-finite number as decimal"
    if.
      n.gte 9.223372036854776e18
      T
        "Can't write this number as decimal digits: it is outside the signed 64-bit range"
      if.
        n.lt -9.223372036854776e18
        T
          "Can't write this number as decimal digits: it is outside the signed 64-bit range"
        from-i64
          i64 integer
  [] > from-i64 /Q.bytes
    ? > value /Q.i64
  n.as-i64 > integer!
```

There must be exactly one `n.as-i64`; `integer!` is cached bytes and must be redecorated as `i64 integer` at the application. Do not add digit loops, division, multiplication, chunks, whole-value negation, or production logic to `printf.eo`.

- [ ] **Step 2: Add the exact Java atom**

Create `EOas_decimal$EOfrom_i64.java` with `@XmirObject(oname = "as-decimal.from-i64")`, `public final`, `extends PhDefault`, `implements Atom`, and constructor attributes `new Attrs(new Attr("value", new AtVoid("value")))`. Its lambda is exactly:

```java
@Override
public Phi lambda() {
    final long value = new Dataized(this.take("value")).take(Long.class);
    return new Data.ToPhi(new BytesOf(Long.toString(value)).take());
}
```

Do not return `Data.ToPhi(Long)` or `Data.ToPhi(Number)`, because those paths convert through `double` and lose values above `2^53`.

- [ ] **Step 3: Keep the narrow `as-fixed` helper**

After places validation and before the existing signed/positive rounding path, branch on `n.is-finite.and n.abs.gte 9007199254740992`. Every such value is integral on the IEEE-754 grid, so apply this helper directly to positive or negative `n`; this avoids positive-value double scaling drift as well as whole-value negation of `Long.MIN_VALUE`:

```eo
if. > [a] >> signed-large
  places.eq 0
  as-decimal a
  concat.
    concat.
      as-decimal a
      ".".as-bytes
    rec-pad 0 places --
| n
```

The ordinary path remains byte-for-byte equivalent below `2^53`; non-finite values do not enter the shortcut.

### Task 4: Validate correctness, typing, latency, and scope

**Files:**

- Verify: `eo-runtime/src/test/java/org/eolang/EO_string/EOas_decimalEOfrom_i64Test.java`
- Verify: `eo-runtime/src/main/eo/string/as-decimal.eo`
- Verify: `eo-runtime/src/main/eo/string/as-fixed.eo`
- Verify: `eo-runtime/src/main/eo/string/printf.eo`

- [ ] **Step 1: Run the direct atom test**

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalEOfrom_i64Test' test -pl :eo-runtime
```

Expected: six conversions, wrong-width rejection, and `Φ.bytes` forma all pass.

- [ ] **Step 2: Enforce the isolated `<10s` gate**

```powershell
$Issue6572Watch=[System.Diagnostics.Stopwatch]::StartNew()
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest#can_write_the_long_minimum' test -pl :eo-runtime
$Issue6572Code=$LASTEXITCODE
$Issue6572Watch.Stop()
"Maven wall time: {0:N3}s" -f $Issue6572Watch.Elapsed.TotalSeconds
exit $Issue6572Code
```

Read `eo-runtime/target/surefire-reports/TEST-org.eolang.EO_string.EOas_decimalTest.xml`. Expected: PASS and the testcase `time` is below `10.0`; if not, stop without weakening timeouts.

- [ ] **Step 3: Run all base seams**

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest#can_write_positive_value_below_the_chunk_base+can_write_positive_value_at_the_chunk_base+can_write_positive_value_one_past_the_chunk_base+can_write_positive_value_forty_two_past_the_chunk_base+can_write_negative_value_below_the_chunk_base+can_write_negative_value_at_the_chunk_base+can_write_negative_value_one_past_the_chunk_base+can_write_negative_value_forty_two_past_the_chunk_base' test -pl :eo-runtime
```

Expected: eight seam methods pass.

- [ ] **Step 4: Run the paired extrema**

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest#can_write_the_largest_double_below_the_long_maximum+can_write_the_long_minimum' test -pl :eo-runtime
```

Expected: both methods pass.

- [ ] **Step 5: Run the exact focused classes**

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Dtest=org.eolang.EO_string.EOas_decimalTest,org.eolang.EO_string.EOas_fixedTest,org.eolang.EO_string.EOprintfTest' test -pl :eo-runtime
```

Expected: all focused tests pass with zero failures and zero errors. Record per-class counts and times from the three Surefire XML reports.

- [ ] **Step 6: Verify atom typing**

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-Deo.typing=true' '-Dtest=org.eolang.EO_string.EOas_decimalEOfrom_i64Test,org.eolang.EO_string.EOas_decimalTest,org.eolang.EO_string.EOas_fixedTest,org.eolang.EO_string.EOprintfTest' test -pl :eo-runtime
```

Expected: the direct and focused suites all pass while `AtomTyped` checks the nested `/Q.bytes` declaration.

- [ ] **Step 7: Run module unit tests if the focused gates are green**

```powershell
mvn -o -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' test -pl :eo-runtime
```

Record exact counts. If the known unrelated Windows `SyscallTest` baseline recurs, report it verbatim; do not weaken, skip, or modify that test.

- [ ] **Step 8: Self-review and verify scope**

```powershell
git diff --check
git diff --name-only
git diff -- eo-runtime/src/main/eo/string/as-decimal.eo eo-runtime/src/main/eo/string/as-fixed.eo eo-runtime/src/main/eo/string/printf.eo
git status --short
```

Confirm exact signed decoding, UTF-8 output, no `Data.ToPhi(Number)`, lazy error guards, one `n.as-i64`, `Long.MIN_VALUE`, negative-to-zero behavior, canonical EO, Java style, typing compatibility, explicit Node debt, no log artifacts, and no owned Maven process.

### Task 5: Commit production only and hand off for review

**Files:**

- Stage: `eo-runtime/src/main/eo/string/as-decimal.eo`
- Stage: `eo-runtime/src/main/eo/string/as-fixed.eo`
- Stage: `eo-runtime/src/main/java/org/eolang/EO_string/EOas_decimal$EOfrom_i64.java`

- [ ] **Step 1: Stage and inspect only production files**

```powershell
git add -- eo-runtime/src/main/eo/string/as-decimal.eo eo-runtime/src/main/eo/string/as-fixed.eo 'eo-runtime/src/main/java/org/eolang/EO_string/EOas_decimal$EOfrom_i64.java'
git diff --cached --name-only
git diff --cached --check
git diff --cached
```

Expected: exactly the two EO production files and the Java atom are staged. The direct test is already committed, and `printf.eo` has no new production diff.

- [ ] **Step 2: Commit the verified implementation**

```powershell
git commit -m '#6572: format signed longs natively'
```

- [ ] **Step 3: Inspect the branch without publishing it**

```powershell
git diff upstream/master...HEAD --check
git status --short --branch
git diff --stat upstream/master...HEAD
git log --oneline --decorate upstream/master..HEAD
```

Expected: documentation, committed EO regressions, the direct atom test, and the three production files are present; the worktree is clean. Do not push or open a pull request from this task.
