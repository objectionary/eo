/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Transition}.
 * @since 0.1
 */
@SuppressWarnings("PMD.UnnecessaryLocalRule")
final class TransitionTest {

    @Test
    void pushesFreshLevelOntoEmptyStack() {
        final Stack stack = new Stack();
        final Level level = new Transition(stack, new Span("alpha", 1))
            .apply(Kind.HEAD, Openness.OPEN, null);
        MatcherAssert.assertThat(
            "the first apply on an empty stack must push a level whose kind matches the request",
            level.kind(),
            Matchers.equalTo(Kind.HEAD)
        );
    }

    @Test
    void pushesDeeperLevelWhenIndentStepsByExactlyTwo() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("beta", 1))
            .apply(Kind.BARE_FORMATION, Openness.OPEN, null);
        final Level level = new Transition(stack, new Span("  gamma", 2))
            .apply(Kind.HEAD, Openness.OPEN, null);
        MatcherAssert.assertThat(
            "applying at deeper indent must produce a level whose parent kind matches the stack top",
            level.parent(),
            Matchers.equalTo(Kind.BARE_FORMATION)
        );
    }

    @Test
    void rejectsIndentJumpGreaterThanOneLevel() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("delta", 1))
            .apply(Kind.BARE_FORMATION, Openness.OPEN, null);
        Assertions.assertThrows(
            ParseError.class,
            () -> new Transition(stack, new Span("    epsilon", 2))
                .apply(Kind.HEAD, Openness.OPEN, null),
            "indent jump of four spaces from indent zero must be rejected"
        );
    }

    @Test
    void rejectsDeeperChildUnderHorizontallyCompletedParent() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("zeta", 1))
            .apply(Kind.HAPPLICATION, Openness.HORIZONTAL_COMPLETED, null);
        Assertions.assertThrows(
            ParseError.class,
            () -> new Transition(stack, new Span("  eta", 2))
                .apply(Kind.HEAD, Openness.OPEN, null),
            "a horizontally-completed parent cannot accept a deeper-indent child"
        );
    }

    @Test
    void replacesLevelWhenLineAtSameIndentArrives() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("theta", 1))
            .apply(Kind.HEAD, Openness.OPEN, null);
        final Level level = new Transition(stack, new Span("iota", 2))
            .apply(Kind.HAPPLICATION, Openness.HORIZONTAL_COMPLETED, null);
        MatcherAssert.assertThat(
            "applying at the same indent must replace the top level's kind in place",
            level.kind(),
            Matchers.equalTo(Kind.HAPPLICATION)
        );
    }

    @Test
    void marksLevelAsNamedWhenLabelIsGiven() {
        final Stack stack = new Stack();
        final Level level = new Transition(stack, new Span("kappa", 1))
            .apply(Kind.HEAD, Openness.OPEN, "mu");
        MatcherAssert.assertThat(
            "applying with a non-null label must record the level as carrying a name suffix",
            level.named(),
            Matchers.is(true)
        );
    }

    @Test
    void leavesLevelUnnamedWhenLabelIsNull() {
        final Stack stack = new Stack();
        final Level level = new Transition(stack, new Span("lambda", 1))
            .apply(Kind.HEAD, Openness.OPEN, null);
        MatcherAssert.assertThat(
            "applying with a null label must leave the level without a name flag",
            level.named(),
            Matchers.is(false)
        );
    }
}
