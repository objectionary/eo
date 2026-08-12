# Character Class Glob Translation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `directory.walk` correctly support range, negated, and metacharacter-bearing glob character classes and remove the resolved `#6482` puzzle.

**Architecture:** Extend the existing recursive, single-pass glob-to-regex translator with one `classed` state flag. The same EO source file owns the translator and its embedded behavioral tests, so no public interface or additional object is introduced.

**Tech Stack:** EO, `eo-maven-plugin`, Maven, embedded EO tests

---

## File Structure

- Modify `eo-runtime/src/main/eo/directory.eo`: add embedded character-class tests, extend `translated`, and remove the resolved puzzle.
- Keep `docs/superpowers/specs/2026-08-11-character-class-glob-design.md`: approved design record.
- Keep `docs/superpowers/plans/2026-08-11-character-class-glob.md`: executable TDD plan.

### Task 1: Prepare the focused runtime test environment

**Files:**
- No source changes.

- [ ] **Step 1: Install the current reactor-built plugin without running the unrelated parser tests**

Run:

```powershell
mvn -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' '-DskipTests' install -pl :eo-maven-plugin -am
```

Expected: `BUILD SUCCESS`; the locally installed `eo-maven-plugin:1.0-SNAPSHOT`
contains the current `inference` goal. The known full-reactor `XmirTest` MANIFEST
failure is bypassed because tests are skipped only for this dependency-install step.

- [ ] **Step 2: Confirm the branch contains only the approved documentation commits**

Run:

```powershell
git status -sb
git diff upstream/master...HEAD --stat
```

Expected: a clean worktree whose committed diff contains only the design and
plan files.

### Task 2: Add failing character-class behavior tests

**Files:**
- Modify and test: `eo-runtime/src/main/eo/directory.eo:394-449`

- [ ] **Step 1: Add three embedded tests before the plain-glob test**

Insert the following tests:

```eo
  ++> can-walk-with-a-character-class-range
    seq * > @
      touched.
        as-file.
          d.resolved "a.txt"
      touched.
        as-file.
          d.resolved "1.txt"
      eq.
        string.joined
          ","
          d.as-dir.walk "[a-z].txt"
        d.resolved "a.txt"
    as-path. > d
      made.
        directory mktemp.tmpfile.deleted

  ++> can-walk-with-a-negated-character-class
    seq * > @
      touched.
        as-file.
          d.resolved "a.txt"
      touched.
        as-file.
          d.resolved "b.txt"
      eq.
        string.joined
          ","
          d.as-dir.walk "[!a].txt"
        d.resolved "b.txt"
    as-path. > d
      made.
        directory mktemp.tmpfile.deleted

  ++> can-keep-a-star-literal-inside-a-character-class
    seq * > @
      touched.
        as-file.
          d.resolved "a.txt"
      touched.
        as-file.
          d.resolved "b.txt"
      eq.
        string.joined
          ","
          d.as-dir.walk "[a*].txt"
        d.resolved "a.txt"
    as-path. > d
      made.
        directory mktemp.tmpfile.deleted
```

- [ ] **Step 2: Run the runtime tests and verify RED**

Run:

```powershell
mvn -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' clean test -pl :eo-runtime
```

Expected: `BUILD FAILURE` from the newly added embedded tests because the
existing translator sends character-class contents through wildcard and brace
translation. Confirm the failure names or generated source point to the new
tests, rather than an EO syntax or formatting error.

### Task 3: Translate character classes in one pass

**Files:**
- Modify: `eo-runtime/src/main/eo/directory.eo:3-8`
- Modify: `eo-runtime/src/main/eo/directory.eo:133-207`

- [ ] **Step 1: Remove only the resolved puzzle**

Delete the complete `# @todo #6482:45min` paragraph. Leave the `#5751` and
`#6484` puzzle paragraphs unchanged.

- [ ] **Step 2: Pass initial character-class state into the translator**

Change:

```eo
                    translated 0 "" false
```

to:

```eo
                    translated 0 "" false false
```

- [ ] **Step 3: Extend `translated` with character-class state**

Replace the existing `translated` object with this state-aware version:

```eo
    [index acc braced classed] >> translated
      if. > @
        index.gte glob.length
        acc
        translated
          index.plus
            double.if 2 1
          string.joined *1
            ""
            acc
            token
          if.
            classed
            braced
            if.
              char.eq "{"
              true
              if.
                char.eq "}"
                false
                braced
          if.
            classed
            char.eq "]".not
            char.eq "["
      if. > alternated
        classed
        escaped char
        if.
          char.eq "{"
          "("
          if.
            braced.and
              char.eq "}"
            ")"
            if.
              braced.and
                char.eq ","
              "|"
              escaped char
      string.at glob index > char
      and. > double
        classed.not
        and.
          char.eq "*"
          eq.
            string.at
              glob
              index.plus 1
              "" > [message]
            "*"
      if. > token
        classed
        if.
          and.
            char.eq "!"
            eq.
              string.at glob (index.minus 1)
              "["
          "^"
          escaped char
        double.if
          ".*"
          if.
            char.eq "*"
            string.joined *1
              ""
              single
              "*"
            if.
              char.eq "?"
              single
              if.
                char.eq "/"
                literal
                alternated
```

The current character is translated according to the state on entry. The two
state arguments passed recursively describe the next character: brace state is
frozen while inside a class, and class state opens after `[` and closes after
`]`.

- [ ] **Step 4: Run the focused tests and verify GREEN**

Run:

```powershell
mvn -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' clean test -pl :eo-runtime
```

Expected: `BUILD SUCCESS`, including the three new character-class tests and
the existing malformed `[` test.

- [ ] **Step 5: Let the project formatter canonicalize the EO source**

Run:

```powershell
mvn -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' eo:format -pl :eo-runtime
git diff --check
```

Expected: the formatter exits successfully and `git diff --check` reports no
whitespace errors. If the formatter changes binding order or vertical layout,
retain its canonical output and rerun Step 4.

### Task 4: Verify scope and publish

**Files:**
- Verify: `eo-runtime/src/main/eo/directory.eo`
- Verify: `docs/superpowers/specs/2026-08-11-character-class-glob-design.md`
- Verify: `docs/superpowers/plans/2026-08-11-character-class-glob.md`

- [ ] **Step 1: Run final runtime validation**

Run:

```powershell
mvn -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' clean test -pl :eo-runtime
```

Expected: `BUILD SUCCESS` with zero test failures.

- [ ] **Step 2: Verify the puzzle removal and inspect the complete diff**

Run:

```powershell
rg -n '#6482|character class' eo-runtime/src/main/eo/directory.eo
git diff --check
git status -sb
git diff upstream/master...HEAD
```

Expected: no `#6482` puzzle remains; only the approved docs, tests, and
translator changes are present; `git diff --check` is clean.

- [ ] **Step 3: Commit the implementation**

Run:

```powershell
git add eo-runtime/src/main/eo/directory.eo
git commit -m '#6549: translate glob character classes'
```

Expected: one implementation commit containing the puzzle removal, regression
tests, and translator change.

- [ ] **Step 4: Re-run final validation after the commit**

Run:

```powershell
mvn -ntp -PskipITs --errors --batch-mode '-Deo.coverageTracking=false' test -pl :eo-runtime
git status -sb
```

Expected: `BUILD SUCCESS`; the worktree is clean and ahead of
`upstream/master` only by the intended commits.

- [ ] **Step 5: Push and open a draft PR**

Push `agent/issue-6549-character-class-glob` to the authenticated fork, then
open a draft pull request against `objectionary/eo:master`. The PR body must
describe the character-class state machine, the fixed range/negation/star
behaviors, the removed puzzle, validation evidence, and the unrelated baseline
MANIFEST limitation encountered during full-reactor testing.
