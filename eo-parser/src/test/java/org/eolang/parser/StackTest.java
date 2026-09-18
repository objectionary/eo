/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Stack}.
 *
 * @since 0.1
 */
final class StackTest {

    @Test
    void startsEmpty() {
        MatcherAssert.assertThat(
            "a freshly constructed stack must be empty",
            new Stack().empty(),
            Matchers.is(true)
        );
    }

    @Test
    void pushesTopLevelEntryAtIndentZero() {
        MatcherAssert.assertThat(
            "the first push must produce an entry whose parent is TOP_LEVEL",
            new Stack().push(0, 1, Kind.BARE_FORMATION, Openness.OPEN).parent(),
            Matchers.equalTo(Kind.TOP_LEVEL)
        );
    }

    @Test
    void rejectsFirstPushAtNonZeroIndent() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Stack().push(2, 1, Kind.HEAD, Openness.OPEN),
            "first push must be at indent 0 — non-zero indent cannot start a program"
        );
    }

    @Test
    void capturesLineOfFirstPushIndentViolation() {
        MatcherAssert.assertThat(
            "the error must carry the offending line",
            StackTest.firstPushIndentViolation().line(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void capturesColumnOfFirstPushIndentViolation() {
        MatcherAssert.assertThat(
            "the error must carry the offending column",
            StackTest.firstPushIndentViolation().pos(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void capturesMessageOfFirstPushIndentViolation() {
        MatcherAssert.assertThat(
            "the error message must name the indent-0 requirement",
            StackTest.firstPushIndentViolation().getMessage(),
            Matchers.equalTo(
                "unexpected indentation, the first object must start at indent 0"
            )
        );
    }

    @Test
    void readsParentKindFromEntryBelow() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        MatcherAssert.assertThat(
            "a pushed child must read parentKind from the entry directly below",
            stack.push(2, 2, Kind.HEAD, Openness.OPEN).parent(),
            Matchers.equalTo(Kind.BARE_FORMATION)
        );
    }

    @Test
    void propagatesAtomFlagToChildren() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN).mark();
        MatcherAssert.assertThat(
            "a child of an atom must see parentAtom() == true",
            stack.push(2, 2, Kind.HEAD, Openness.OPEN).patom(),
            Matchers.is(true)
        );
    }

    @Test
    void popsDeeperLevelsAndRunsCloser() {
        final List<Integer> closed = new ArrayList<>(0);
        final Stack stack = new Stack((level, naming) -> closed.add(level.indent()));
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        stack.push(4, 3, Kind.HEAD, Openness.OPEN);
        stack.popDeeperThan(0);
        MatcherAssert.assertThat(
            "popDeeperThan must close every entry above the target indent",
            closed,
            Matchers.contains(4, 2)
        );
    }

    @Test
    void downgradesTopOpennessOnPopStep() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        stack.popDeeperThan(0);
        MatcherAssert.assertThat(
            "after popping a deeper level, the surviving top must drop to VCOMPLETED",
            stack.top().openness(),
            Matchers.equalTo(Openness.VCOMPLETED)
        );
    }

    @Test
    void leavesHorizontallyCompletedTopAlone() {
        final Stack stack = new Stack();
        final Level top = stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        top.close(Openness.HCOMPLETED);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        stack.popDeeperThan(0);
        MatcherAssert.assertThat(
            "a horizontally-completed top must not be downgraded to vertical-completed",
            stack.top().openness(),
            Matchers.equalTo(Openness.HCOMPLETED)
        );
    }

    @Test
    void replacesTopAndClosesOld() {
        final List<Integer> closed = new ArrayList<>(0);
        final Stack stack = new Stack((level, naming) -> closed.add(level.start()));
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        stack.replace(5, Kind.BARE_FORMATION, Openness.OPEN);
        MatcherAssert.assertThat(
            "replace must close the old top through the supplied closer",
            closed,
            Matchers.contains(2)
        );
    }

    @Test
    void inheritsIndentFromOldTopOnReplacement() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        MatcherAssert.assertThat(
            "the replacement entry must occupy the indent of the entry it replaced",
            stack.replace(5, Kind.BARE_FORMATION, Openness.OPEN).indent(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void runsCloserOnEveryRemainingEntryAtClose() {
        final List<Integer> closed = new ArrayList<>(0);
        final Stack stack = new Stack((level, naming) -> closed.add(level.indent()));
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        stack.close();
        MatcherAssert.assertThat(
            "close must pop every entry top-first and invoke the closer for each",
            closed,
            Matchers.contains(2, 0)
        );
    }

    @Test
    void leavesStackEmptyAfterClose() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        stack.close();
        MatcherAssert.assertThat(
            "after close() the stack cannot retain any entry",
            stack.empty(),
            Matchers.is(true)
        );
    }

    @Test
    void reinstatesTheEntryDisplacedByReplaceOnRestore() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        final Level displaced = stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        final List<Level> snapshot = stack.snapshot();
        stack.replace(3, Kind.COMPACT_TUPLE, Openness.OPEN);
        stack.restore(snapshot);
        MatcherAssert.assertThat(
            "restore must bring back the entry replace displaced"
                .concat(", not merely one at the same indent"),
            stack.top().start(),
            Matchers.equalTo(displaced.start())
        );
    }

    @Test
    void undoesMutationOfSurvivingEntryOnRestore() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_REVERSED, Openness.OPEN);
        final List<Level> snapshot = stack.snapshot();
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        stack.restore(snapshot);
        MatcherAssert.assertThat(
            "restore must undo the receiver the failed child consumed"
                .concat(", not merely drop that child"),
            stack.top().taken(),
            Matchers.is(false)
        );
    }

    @Test
    void answersTopLevelBelowAnEmptyStack() {
        MatcherAssert.assertThat(
            "the entry below an empty stack must be the bottom sentinel, whose kind is TOP_LEVEL",
            new Stack().below().kind(),
            Matchers.equalTo(Kind.TOP_LEVEL)
        );
    }

    @Test
    void answersTopLevelBelowTheOnlyEntry() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.HEAD, Openness.OPEN);
        MatcherAssert.assertThat(
            "the entry below the only entry must be the bottom sentinel, whose kind is TOP_LEVEL",
            stack.below().kind(),
            Matchers.equalTo(Kind.TOP_LEVEL)
        );
    }

    @Test
    void keepsTheBottomSentinelNonArgumentative() {
        MatcherAssert.assertThat(
            "the bottom sentinel cannot put its children in only-phi argument position",
            new Stack().below().argumentative(),
            Matchers.is(false)
        );
    }

    @Test
    void answersTheRealEntryBelowTheTop() {
        final Stack stack = new Stack();
        final Level under = stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN);
        MatcherAssert.assertThat(
            "the entry below the top must be the one pushed right before it",
            stack.below(),
            Matchers.sameInstance(under)
        );
    }

    private static ParseError firstPushIndentViolation() {
        return Assertions.assertThrows(
            ParseError.class,
            () -> new Stack().push(2, 1, Kind.HEAD, Openness.OPEN)
        );
    }
}
