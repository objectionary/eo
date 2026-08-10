# Inline Label Binding Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve an inline `:label` on its owning application when XMIR is printed and reparsed.

**Architecture:** Keep the existing line-tree model and penalty selector. Remove the unsafe horizontal candidate only when a non-leaf node's tail starts with `:`, allowing the existing vertical renderer to place the label next to its owner.

**Tech Stack:** Java 17+, Maven, JUnit 5 parameterized tests, EO printer YAML fixtures

---

### Task 1: Preserve labels on nested applications

**Files:**
- Create: `eo-printer/src/test/resources/org/eolang/printer/print-packs/yaml/inline-label-binding.yaml`
- Modify: `eo-printer/src/main/java/org/eolang/printer/Pretty.java:274-297`

- [ ] **Step 1: Add the regression fixture**

```yaml
# SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
# SPDX-License-Identifier: MIT
---
penalties:
  INDENT: 3
  BRACKET: 7
  EXCESS: 1
  WIDTH: 80
  STEP: 2
  SPACE: 7
origin: |
  a (b c):lbl > x

printed: |
  a > x
    b:lbl
      c
```

- [ ] **Step 2: Run the exact-output test and verify RED**

Run: `mvn -pl eo-printer test "-Dtest=XmirTest#printsToEo" "-Dinvoker.skip=true" --batch-mode --no-transfer-progress`

Expected: FAIL for `inline-label-binding.yaml`, showing actual output `b c:lbl` instead of the expected vertical `b:lbl` with child `c`.

- [ ] **Step 3: Reject the unsafe horizontal candidate**

In `Pretty.horizontal()`, replace the leaf-only guard with:

```java
        } else if (node.children.isEmpty() || node.tail.startsWith(":")) {
            result = Optional.empty();
```

Extend the method Javadoc to state that a `:label` tail cannot follow inlined arguments because it would bind to the last argument rather than the node.

- [ ] **Step 4: Verify GREEN and round-trip behavior**

Run: `mvn -pl eo-printer test "-Dtest=XmirTest" "-Dinvoker.skip=true" --batch-mode --no-transfer-progress`

Expected: BUILD SUCCESS; the fixture passes `printsToEo` and `printsToParseableEo`, proving exact safe output and stable reparsing.

- [ ] **Step 5: Run module verification**

Run: `mvn -pl eo-printer -am clean install "-Dinvoker.skip=true" "-DskipITs=true" --batch-mode --no-transfer-progress`

Run: `mvn -pl eo-printer -am verify -PskipTests -Pqulice "-Dinvoker.skip=true" --batch-mode --no-transfer-progress`

Expected: both commands finish with BUILD SUCCESS.

- [ ] **Step 6: Check scope and commit**

Run: `git diff --check && git diff --numstat`

Expected: only the fixture and `Pretty.java` are modified, and total additions plus deletions for the complete branch remain at or below 200.

```bash
git add eo-printer/src/test/resources/org/eolang/printer/print-packs/yaml/inline-label-binding.yaml eo-printer/src/main/java/org/eolang/printer/Pretty.java
git commit -m "#6563: preserve inline label binding"
```
