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
final class TransitionTest {

    @Test
    void pushesFreshLevelOntoEmptyStack() {
        MatcherAssert.assertThat(
            "the first apply on an empty stack must push a level whose kind matches the request",
            new Transition(new Stack(), new Span("alpha", 1), new Emit())
                .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false))
                .kind(),
            Matchers.equalTo(Kind.HEAD)
        );
    }

    @Test
    void pushesDeeperLevelWhenIndentStepsByExactlyTwo() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("beta", 1), new Emit())
            .apply(Kind.BARE_FORMATION, Openness.OPEN, new Admission(null, false));
        MatcherAssert.assertThat(
            "applying at deeper indent must produce a level whose parent kind matches the stack top",
            new Transition(stack, new Span("  gamma", 2), new Emit())
                .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false))
                .parent(),
            Matchers.equalTo(Kind.BARE_FORMATION)
        );
    }

    @Test
    void promotesHeadToVapplicationWhenDeeperChildArrives() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("nu", 1), new Emit())
            .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false));
        new Transition(stack, new Span("  xi", 2), new Emit())
            .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false));
        MatcherAssert.assertThat(
            "a head must promote to vapplication once its first deeper-indent child pushes",
            stack.below().kind(),
            Matchers.equalTo(Kind.VAPPLICATION)
        );
    }

    @Test
    void promotesHmethodToVapplicationWhenDeeperChildArrives() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("omicron", 1), new Emit())
            .apply(Kind.HMETHOD, Openness.OPEN, new Admission(null, false));
        new Transition(stack, new Span("  pi", 2), new Emit())
            .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false));
        MatcherAssert.assertThat(
            "an hmethod must promote to vapplication once its first deeper-indent child pushes",
            stack.below().kind(),
            Matchers.equalTo(Kind.VAPPLICATION)
        );
    }

    @Test
    void rejectsIndentJumpGreaterThanOneLevel() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("delta", 1), new Emit())
            .apply(Kind.BARE_FORMATION, Openness.OPEN, new Admission(null, false));
        Assertions.assertThrows(
            ParseError.class,
            () -> new Transition(stack, new Span("    epsilon", 2), new Emit())
                .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false)),
            "indent jump of four spaces from indent zero must be rejected"
        );
    }

    @Test
    void capturesCanonicalMessageOfIndentJumpViolation() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("delta", 1), new Emit())
            .apply(Kind.BARE_FORMATION, Openness.OPEN, new Admission(null, false));
        MatcherAssert.assertThat(
            "the error message must name the indent-step requirement",
            Assertions.assertThrows(
                ParseError.class,
                () -> new Transition(stack, new Span("    epsilon", 2), new Emit())
                    .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false))
            ).getMessage(),
            Matchers.equalTo("indent increased by more than one level")
        );
    }

    @Test
    void rejectsDeeperChildUnderHorizontallyCompletedParent() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("zeta", 1), new Emit())
            .apply(Kind.HAPPLICATION, Openness.HCOMPLETED, new Admission(null, false));
        Assertions.assertThrows(
            ParseError.class,
            () -> new Transition(stack, new Span("  eta", 2), new Emit())
                .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false)),
            "a horizontally-completed parent cannot accept a deeper-indent child"
        );
    }

    @Test
    void rejectsAnyDisallowedChildUnderAnAtomRegardlessOfLineShape() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("theta", 1), new Emit())
            .apply(Kind.BARE_FORMATION, Openness.OPEN, new Admission(null, false))
            .mark();
        Assertions.assertThrows(
            ParseError.class,
            () -> this.happlicationChild(stack, false),
            "an application child under an atom parent must be rejected, same as a formation child"
        );
    }

    @Test
    void permitsAPermittedChildUnderAnAtom() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("theta", 1), new Emit())
            .apply(Kind.BARE_FORMATION, Openness.OPEN, new Admission(null, false))
            .mark();
        MatcherAssert.assertThat(
            "a permitted child (a test attribute or a void parameter) must be accepted under an atom",
            this.happlicationChild(stack, true).kind(),
            Matchers.equalTo(Kind.HAPPLICATION)
        );
    }

    @Test
    void replacesLevelWhenLineAtSameIndentArrives() {
        final Stack stack = new Stack();
        new Transition(stack, new Span("theta", 1), new Emit())
            .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false));
        MatcherAssert.assertThat(
            "applying at the same indent must replace the top level's kind in place",
            new Transition(stack, new Span("iota", 2), new Emit())
                .apply(Kind.HAPPLICATION, Openness.HCOMPLETED, new Admission(null, false))
                .kind(),
            Matchers.equalTo(Kind.HAPPLICATION)
        );
    }

    @Test
    void marksLevelAsNamedWhenLabelIsGiven() {
        MatcherAssert.assertThat(
            "applying with a non-null label must record the level as carrying a name suffix",
            new Transition(new Stack(), new Span("kappa", 1), new Emit())
                .apply(Kind.HEAD, Openness.OPEN, new Admission("mu", false))
                .named(),
            Matchers.is(true)
        );
    }

    @Test
    void leavesLevelUnnamedWhenLabelIsNull() {
        MatcherAssert.assertThat(
            "applying with a null label must leave the level without a name flag",
            new Transition(new Stack(), new Span("lambda", 1), new Emit())
                .apply(Kind.HEAD, Openness.OPEN, new Admission(null, false))
                .named(),
            Matchers.is(false)
        );
    }

    private Level happlicationChild(final Stack stack, final boolean permitted) {
        return new Transition(stack, new Span("  42", 2), new Emit()).apply(
            Kind.HAPPLICATION, Openness.HCOMPLETED, new Admission(null, permitted)
        );
    }
}
