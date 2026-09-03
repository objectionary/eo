# Issue 8249 Inline-Phi Compact Tuple Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reject compact-tuple markers in parenthesized anonymous inline-phi formations so they cannot become implicit empty-tuple arguments.

**Architecture:** Reuse the established `LnOnlyPhi.compactStar` parser rule in `Emissions.inlinePhi`. When the LHS has a valid trailing `*` or `*N` marker, report a `ParseError` before emission; all other inline-phi expressions continue unchanged.

**Tech Stack:** Java 17, Maven, JUnit 5, YAML/XPath parser packs.

---

### Task 1: Add the failing parser regression pack

**Files:**
- Create: `eo-parser/src/test/resources/org/eolang/parser/eo-syntax/inline-phi-compact-tuple-inside-parens-is-rejected.yaml`
- Test: `eo-parser/src/test/java/org/eolang/parser/EoSyntaxTest.java`

- [ ] **Step 1: Write the failing test**

Create this YAML pack:

```yaml
# SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
# SPDX-License-Identifier: MIT
---
sheets: []
asserts:
  - /object/errors/error[contains(text(),'compact tuple marker is not allowed inside a parenthesised inline-phi')]
input: |
  [] > foo
    bar (seq * > [m]) > baz
```

- [ ] **Step 2: Run the focused test to verify it fails**

Run:

```powershell
mvn -pl eo-parser '-Dtest=EoSyntaxTest' -DskipITs test
```

Expected: the new pack fails because the current parser emits no error for the compact-tuple marker.

### Task 2: Reject the unsupported compact-tuple marker

**Files:**
- Modify: `eo-parser/src/main/java/org/eolang/parser/LnOnlyPhi.java:255-265`
- Modify: `eo-parser/src/main/java/org/eolang/parser/Emissions.java:475-508`
- Test: `eo-parser/src/test/resources/org/eolang/parser/eo-syntax/inline-phi-compact-tuple-inside-parens-is-rejected.yaml`

- [ ] **Step 1: Make the compact-marker recognizer reusable**

Change the signature in `LnOnlyPhi` from:

```java
private static int compactStar(final String lhs, final Span span) {
```

to:

```java
static int compactStar(final String lhs, final Span span) {
```

- [ ] **Step 2: Add the guard before inline-phi emission**

Immediately after constructing `sub` in `Emissions.inlinePhi`, add:

```java
if (LnOnlyPhi.compactStar(lhs, sub) >= 0) {
    throw new ParseError(
        line, column + lhs.lastIndexOf('*'),
        "compact tuple marker is not allowed inside a parenthesised inline-phi"
    );
}
```

Keep construction of `Tokens`, `Emissions.expression`, and the existing end-of-input validation after this guard unchanged.

- [ ] **Step 3: Run the focused test to verify it passes**

Run:

```powershell
mvn -pl eo-parser '-Dtest=EoSyntaxTest' -DskipITs test
```

Expected: `EoSyntaxTest` passes, including the new rejection pack.

### Task 3: Verify the focused and module suites

**Files:**
- Verify: `eo-parser/src/main/java/org/eolang/parser/Emissions.java`
- Verify: `eo-parser/src/main/java/org/eolang/parser/LnOnlyPhi.java`
- Verify: `eo-parser/src/test/resources/org/eolang/parser/eo-syntax/inline-phi-compact-tuple-inside-parens-is-rejected.yaml`

- [ ] **Step 1: Run focused compact-tuple and inline-phi tests**

Run:

```powershell
mvn -pl eo-parser '-Dtest=EoSyntaxTest,LnOnlyPhiTest' -DskipITs test
```

Expected: both suites pass, confirming line-level compact tuples still work.

- [ ] **Step 2: Run the complete parser module suite**

Run:

```powershell
mvn -pl eo-parser -DskipITs test
```

Expected: Maven exits with code 0 and reports zero failures and errors.

- [ ] **Step 3: Review the final change set**

Run:

```powershell
git diff --check
git status --short
```

Expected: no whitespace errors; only the issue-8249 implementation files are modified or added.

### Task 4: Commit the implementation

**Files:**
- Modify: `eo-parser/src/main/java/org/eolang/parser/LnOnlyPhi.java`
- Modify: `eo-parser/src/main/java/org/eolang/parser/Emissions.java`
- Create: `eo-parser/src/test/resources/org/eolang/parser/eo-syntax/inline-phi-compact-tuple-inside-parens-is-rejected.yaml`

- [ ] **Step 1: Commit the verified fix**

Run:

```powershell
git add -- eo-parser/src/main/java/org/eolang/parser/LnOnlyPhi.java eo-parser/src/main/java/org/eolang/parser/Emissions.java eo-parser/src/test/resources/org/eolang/parser/eo-syntax/inline-phi-compact-tuple-inside-parens-is-rejected.yaml
git commit -m "fix: reject compact tuple marker inside parenthesized inline phi"
```

Expected: one commit contains only the parser fix and its regression pack.
