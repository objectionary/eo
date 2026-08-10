# Merge-monikers Hot-path Optimization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the three remaining local constant-factor costs identified in #6512 without changing printer output.

**Architecture:** Keep the existing key-based lookup and template-priority design. Guard expensive match predicates with cheap node-shape checks, retrieve the already-proven hosted binding directly in the selected template, and conditionally sort dispatch candidates only when ordering work is necessary.

**Tech Stack:** XSLT 2.0, Saxon-HE 13.0, Java 8, JUnit 5, jcabi XML matchers, Maven.

---

## File Structure

- Modify `eo-printer/src/test/java/org/eolang/printer/XmirTest.java`: structural regression tests for the stylesheet's hot-path contract.
- Modify `eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl`: guarded templates, a direct hosted-binding key lookup, and conditional dispatch sorting.
- Keep `docs/superpowers/specs/2026-08-11-merge-monikers-hot-path-design.md` as the approved scope and acceptance criteria.

### Task 0: Capture the pre-change synthetic baseline

**Files:**
- Temporarily create, then remove: `eo-printer/src/test/java/org/eolang/printer/MergeMonikersBenchmarkTest.java`

- [ ] **Step 1: Add a temporary synthetic benchmark test**

Create this uncommitted test:

```java
/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Temporary local benchmark for issue #6512.
 * @since 0.60.0
 */
final class MergeMonikersBenchmarkTest {

    @Test
    void measuresOneLargeFormation() {
        final StringBuilder source = new StringBuilder("<object><o name='main'>");
        for (int idx = 0; idx < 1000; ++idx) {
            source.append("<o base='Q.x' name='a🌵").append(idx).append("'/>");
            source.append("<o base='ξ.a🌵").append(idx).append("'/>");
        }
        final XML input = new XMLDocument(source.append("</o></object>").toString());
        final Xsline sheet = new Xsline(
            new StClasspath("/org/eolang/printer/print/merge-monikers.xsl")
        );
        for (int warm = 0; warm < 3; ++warm) {
            sheet.pass(input);
        }
        long best = Long.MAX_VALUE;
        XML output = input;
        for (int run = 0; run < 7; ++run) {
            final long start = System.nanoTime();
            output = sheet.pass(input);
            best = Math.min(best, System.nanoTime() - start);
        }
        MatcherAssert.assertThat(
            "The synthetic transformation must produce output",
            output.toString(),
            Matchers.not(Matchers.emptyString())
        );
        System.out.printf("merge-monikers-1000-pairs-best-ms=%.3f%n", best / 1_000_000.0d);
    }
}
```

- [ ] **Step 2: Run and record the baseline**

Run:

```powershell
mvn -pl eo-printer -Dtest=MergeMonikersBenchmarkTest test
```

Expected: BUILD SUCCESS and a `merge-monikers-1000-pairs-best-ms=<value>` line in the test output or Surefire report.

- [ ] **Step 3: Remove the temporary test**

Delete `MergeMonikersBenchmarkTest.java` with `apply_patch` and confirm `git status --short` contains no benchmark file. Retain the recorded value for the PR body.

### Task 1: Avoid repeating the hosted first-reference lookup

**Files:**
- Modify: `eo-printer/src/test/java/org/eolang/printer/XmirTest.java`
- Modify: `eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl:405,520`

- [ ] **Step 1: Write the failing hosted-lookup contract test**

Add the stylesheet loader and first test to the existing test class:

```java
/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.util.Objects;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for the merge-monikers stylesheet.
 * @since 0.60.0
 */
final class XmirTest {

    /** Stylesheet under test. */
    private final XML sheet;

    /**
     * New test instance.
     * @throws IOException If the stylesheet cannot be read
     */
    XmirTest() throws IOException {
        this.sheet = new XMLDocument(
            Objects.requireNonNull(
                XmirTest.class.getResourceAsStream(
                    "/org/eolang/printer/print/merge-monikers.xsl"
                )
            )
        );
    }

    @Test
    void avoidsRepeatingHostedLookup() {
        MatcherAssert.assertThat(
            "The hosted template must not repeat the full first-host lookup",
            this.sheet,
            XhtmlMatchers.hasXPaths(
                "/*/*[local-name()='template' and @priority='1']/*[local-name()='variable' and @name='owner' and @select='ancestor::o[eo:abstract(.)][1]']",
                "/*/*[local-name()='template' and @priority='1']/*[local-name()='variable' and @name='binding' and @select=\"key('moniker-binding', concat(generate-id($owner), ' ', eo:resolved-ref(.)), root(.))[1]\"]"
            )
        );
    }
}
```

- [ ] **Step 2: Run the test and verify RED**

Run:

```powershell
mvn -pl eo-printer -Dtest=XmirTest test
```

Expected: FAIL because the hosted template body still calls `eo:hosted-binding(.)` a second time.

- [ ] **Step 3: Retrieve the proven binding directly**

Keep the first-host proof in the guarded pattern, then use the existing key in the body:

```xml
<xsl:template match="o[starts-with(@base, $eo:xi-dot)][exists(eo:hosted-binding(.))]" priority="1">
  <xsl:variable name="owner" select="ancestor::o[eo:abstract(.)][1]"/>
  <xsl:variable name="binding" select="key('moniker-binding', concat(generate-id($owner), ' ', eo:resolved-ref(.)), root(.))[1]"/>
  ...
</xsl:template>
```

- [ ] **Step 4: Run the focused test and verify GREEN**

Run:

```powershell
mvn -pl eo-printer -Dtest=XmirTest test
```

Expected: the hosted-lookup test passes with 0 failures and BUILD SUCCESS.

- [ ] **Step 5: Commit the single-evaluation change**

```powershell
git add -- eo-printer/src/test/java/org/eolang/printer/XmirTest.java eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl
git commit -m '#6512: avoid repeating hosted first-reference lookup'
```

### Task 2: Reject irrelevant nodes before expensive template predicates

**Files:**
- Modify: `eo-printer/src/test/java/org/eolang/printer/XmirTest.java`
- Modify: `eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl:405,520`

- [ ] **Step 1: Add the failing guard contract test**

Add this method to `XmirTest`:

```java
@Test
void guardsExpensiveTemplatePredicates() {
    MatcherAssert.assertThat(
        "Cheap predicates must reject nodes before hosted/applied lookups",
        this.sheet,
        XhtmlMatchers.hasXPaths(
            "/*/*[local-name()='template' and @match=\"o[starts-with(@base, $eo:xi-dot)][exists(eo:hosted-binding(.))]\"]",
            "/*/*[local-name()='template' and @match=\"o[starts-with(@base, $eo:xi-dot)][exists(o)][not(exists(@name))][exists(eo:applied-handle(.))]\"]"
        )
    );
}
```

- [ ] **Step 2: Run the method and verify RED**

Run:

```powershell
mvn -pl eo-printer '-Dtest=XmirTest#guardsExpensiveTemplatePredicates' test
```

Expected: FAIL because both match patterns still contain their expensive function predicate.

- [ ] **Step 3: Add the necessary cheap predicates first**

Replace the two template declarations with:

```xml
<xsl:template match="o[starts-with(@base, $eo:xi-dot)][exists(eo:hosted-binding(.))]" priority="1">
...
<xsl:template match="o[starts-with(@base, $eo:xi-dot)][exists(o)][not(exists(@name))][exists(eo:applied-handle(.))]" priority="2">
```

- [ ] **Step 4: Run all structural tests and verify GREEN**

Run:

```powershell
mvn -pl eo-printer -Dtest=XmirTest test
```

Expected: 2 tests, 0 failures, BUILD SUCCESS.

- [ ] **Step 5: Commit the guard change**

```powershell
git add -- eo-printer/src/test/java/org/eolang/printer/XmirTest.java eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl
git commit -m '#6512: guard merge-monikers template lookups'
```

### Task 3: Skip sorting fewer than two dispatch candidates

**Files:**
- Modify: `eo-printer/src/test/java/org/eolang/printer/XmirTest.java`
- Modify: `eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl:166-172`

- [ ] **Step 1: Add the failing conditional-sort contract test**

Add this method to `XmirTest`:

```java
@Test
void sortsOnlyMultipleDispatches() {
    MatcherAssert.assertThat(
        "Dispatch ordering must sort only when at least two candidates exist",
        this.sheet,
        XhtmlMatchers.hasXPaths(
            "/*/*[local-name()='function' and @name='eo:moniker-refs']/*[local-name()='variable' and @name='dispatch']/*[local-name()='choose']/*[local-name()='when' and @test='exists($dispatches[2])']/*[local-name()='perform-sort' and @select='$dispatches']",
            "/*/*[local-name()='function' and @name='eo:moniker-refs']/*[local-name()='variable' and @name='dispatch']/*[local-name()='choose']/*[local-name()='otherwise']/*[local-name()='sequence' and @select='$dispatches']"
        )
    );
}
```

- [ ] **Step 2: Run the method and verify RED**

Run:

```powershell
mvn -pl eo-printer '-Dtest=XmirTest#sortsOnlyMultipleDispatches' test
```

Expected: FAIL because `xsl:perform-sort` currently runs unconditionally.

- [ ] **Step 3: Add the minimal conditional sort**

Replace the current dispatch variable with:

```xml
<xsl:variable name="dispatches" select="$refs[eo:dispatch-seg(.) != '']"/>
<xsl:variable name="dispatch" as="element()*">
  <xsl:choose>
    <xsl:when test="exists($dispatches[2])">
      <xsl:perform-sort select="$dispatches">
        <xsl:sort select="count(tokenize(eo:dispatch-seg(.), '\.'))" data-type="number" order="ascending"/>
      </xsl:perform-sort>
    </xsl:when>
    <xsl:otherwise>
      <xsl:sequence select="$dispatches"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:variable>
```

- [ ] **Step 4: Run all structural tests and verify GREEN**

Run:

```powershell
mvn -pl eo-printer -Dtest=XmirTest test
```

Expected: 3 tests, 0 failures, BUILD SUCCESS.

- [ ] **Step 5: Commit the conditional-sort change**

```powershell
git add -- eo-printer/src/test/java/org/eolang/printer/XmirTest.java eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl
git commit -m '#6512: skip trivial merge-monikers sorts'
```

### Task 4: Verify behavior, performance, quality, and scope

**Files:**
- Verify: `eo-printer/src/test/java/org/eolang/printer/XmirTest.java`
- Verify: `eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl`

- [ ] **Step 1: Run the focused structural tests**

```powershell
mvn -pl eo-printer -Dtest=XmirTest test
```

Expected: 3 tests, 0 failures, BUILD SUCCESS.

- [ ] **Step 2: Run all printer behavior tests**

```powershell
mvn -pl eo-printer test '-DskipITs=true' '-Dinvoker.skip=true'
```

Expected: all tests pass with 0 failures and 0 errors.

- [ ] **Step 3: Run module quality verification**

```powershell
mvn -pl eo-printer verify '-DskipITs=true' '-Dinvoker.skip=true'
```

Expected: BUILD SUCCESS, including compilation, tests, and configured quality checks.

- [ ] **Step 4: Repeat the synthetic measurement**

Temporarily recreate `MergeMonikersBenchmarkTest.java` with the exact code from Task 0, run:

```powershell
mvn -pl eo-printer -Dtest=MergeMonikersBenchmarkTest test
```

Expected: BUILD SUCCESS and a post-change `merge-monikers-1000-pairs-best-ms=<value>`. Record the best time, then remove the temporary test with `apply_patch` and confirm it is absent from `git status --short`.

- [ ] **Step 5: Inspect scope and whitespace**

```powershell
git status -sb
git diff upstream/master...HEAD --check
git diff upstream/master...HEAD --stat
git diff upstream/master...HEAD -- eo-printer/src/main/resources/org/eolang/printer/print/merge-monikers.xsl eo-printer/src/test/java/org/eolang/printer/XmirTest.java
```

Expected: only the approved design/plan, one stylesheet, and one focused test class differ; `git diff --check` emits nothing.

- [ ] **Step 6: Record validation evidence for the pull request**

Capture the exact test counts, Maven exit status, branch name, commit list, and both synthetic best times in the PR body. Describe the optimization as eliminating repeated work and identify the timing as a local, non-CI benchmark rather than a guaranteed threshold.
