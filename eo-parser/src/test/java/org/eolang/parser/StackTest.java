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
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnnecessaryLocalRule"})
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
        final Stack stack = new Stack();
        final Level entry = stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        MatcherAssert.assertThat(
            "the first push must produce an entry whose parent is TOP_LEVEL",
            entry.parent(),
            Matchers.equalTo(Kind.TOP_LEVEL)
        );
    }

    @Test
    void rejectsFirstPushAtNonZeroIndent() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Stack().push(2, 1, Kind.HEAD, Openness.OPEN, false),
            "first push must be at indent 0 — non-zero indent cannot start a program"
        );
    }

    @Test
    void rejectsPushWithIndentJumpGreaterThanTwo() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> stack.push(4, 2, Kind.HEAD, Openness.OPEN, false),
            "indent jump of more than one level cannot push"
        );
    }

    @Test
    void readsParentKindFromEntryBelow() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        final Level child = stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
        MatcherAssert.assertThat(
            "a pushed child must read parentKind from the entry directly below",
            child.parent(),
            Matchers.equalTo(Kind.BARE_FORMATION)
        );
    }

    @Test
    void propagatesAtomFlagToChildren() {
        final Stack stack = new Stack();
        final Level parent = stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        parent.mark();
        final Level child = stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
        MatcherAssert.assertThat(
            "a child of an atom must see parentAtom() == true",
            child.patom(),
            Matchers.is(true)
        );
    }

    @Test
    void popsDeeperLevelsAndRunsCloser() {
        final List<Integer> closed = new ArrayList<>(0);
        final Stack stack = new Stack(level -> closed.add(level.indent()));
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
        stack.push(4, 3, Kind.HEAD, Openness.OPEN, false);
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
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
        stack.popDeeperThan(0);
        MatcherAssert.assertThat(
            "after popping a deeper level, the surviving top must drop to VERTICAL_COMPLETED",
            stack.top().openness(),
            Matchers.equalTo(Openness.VERTICAL_COMPLETED)
        );
    }

    @Test
    void leavesHorizontallyCompletedTopAlone() {
        final Stack stack = new Stack();
        final Level top = stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        top.close(Openness.HORIZONTAL_COMPLETED);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
        stack.popDeeperThan(0);
        MatcherAssert.assertThat(
            "a horizontally-completed top must not be downgraded to vertical-completed",
            stack.top().openness(),
            Matchers.equalTo(Openness.HORIZONTAL_COMPLETED)
        );
    }

    @Test
    void replacesTopAndClosesOld() {
        final List<Integer> closed = new ArrayList<>(0);
        final Stack stack = new Stack(level -> closed.add(level.start()));
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
        stack.replace(5, Kind.BARE_FORMATION, Openness.OPEN, false);
        MatcherAssert.assertThat(
            "replace must close the old top through the supplied closer",
            closed,
            Matchers.contains(2)
        );
    }

    @Test
    void inheritsIndentFromOldTopOnReplacement() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
        final Level fresh = stack.replace(5, Kind.BARE_FORMATION, Openness.OPEN, false);
        MatcherAssert.assertThat(
            "the replacement entry must occupy the indent of the entry it replaced",
            fresh.indent(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void runsCloserOnEveryRemainingEntryAtClose() {
        final List<Integer> closed = new ArrayList<>(0);
        final Stack stack = new Stack(level -> closed.add(level.indent()));
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        stack.push(2, 2, Kind.HEAD, Openness.OPEN, false);
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
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN, false);
        stack.close();
        MatcherAssert.assertThat(
            "after close() the stack cannot retain any entry",
            stack.empty(),
            Matchers.is(true)
        );
    }
}
