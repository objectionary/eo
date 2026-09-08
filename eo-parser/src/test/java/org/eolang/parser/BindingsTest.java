/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Arrays;
import java.util.Collections;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Bindings}.
 *
 * @since 0.1
 */
final class BindingsTest {

    @Test
    void acceptsEmptyArgs() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Collections.emptyList(), new Span("foo", 1)
            ),
            "an empty arg list was rejected by the all-or-nothing rule"
        );
    }

    @Test
    void acceptsSingleArg() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Collections.singletonList(new Value(Value.Kind.IDENTIFIER, "a", 4)),
                new Span("foo a", 1)
            ),
            "a single arg was rejected by the all-or-nothing rule"
        );
    }

    @Test
    void acceptsAllUnboundArgs() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4),
                    new Value(Value.Kind.IDENTIFIER, "b", 6),
                    new Value(Value.Kind.IDENTIFIER, "c", 8)
                ),
                new Span("foo a b c", 1)
            ),
            "a group of all-unbound args was rejected"
        );
    }

    @Test
    void acceptsAllBoundArgs() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, "x"),
                    new Value(Value.Kind.IDENTIFIER, "b", 8, "y")
                ),
                new Span("foo a:x b:y", 1)
            ),
            "a group of all-bound args was rejected"
        );
    }

    @Test
    void acceptsAllBoundArgsWithNumericBinding() {
        final long seed = System.nanoTime();
        final int slot = new Random(seed).nextInt(10);
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, Integer.toString(slot)),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, Integer.toString(slot))
                ),
                new Span(String.format("foo a:%1$d b:%1$d", slot), 1)
            ),
            String.format("a numeric binding is a valid uniform mode, seed is %d", seed)
        );
    }

    @Test
    void acceptsAllBoundArgsWithEmptyLabel() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, ""),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, "")
                ),
                new Span("foo a: b:", 1)
            ),
            "an empty binding label is still a binding, not its absence"
        );
    }

    @Test
    void rejectsEmptyLabelFollowedByUnbound() {
        MatcherAssert.assertThat(
            "an empty label counts as bound, so an unbound successor must be rejected",
            Assertions.assertThrows(
                ParseError.class,
                () -> Bindings.checkAllOrNothing(
                    Arrays.asList(
                        new Value(Value.Kind.IDENTIFIER, "a", 4, ""),
                        new Value(Value.Kind.IDENTIFIER, "b", 6)
                    ),
                    new Span("foo a: b", 1)
                )
            ).pos(),
            Matchers.equalTo(6)
        );
    }

    @Test
    void rejectsMixedBoundAndUnbound() {
        MatcherAssert.assertThat(
            "the divergent arg's column must be reported",
            Assertions.assertThrows(
                ParseError.class,
                () -> Bindings.checkAllOrNothing(
                    Arrays.asList(
                        new Value(Value.Kind.IDENTIFIER, "a", 4, "x"),
                        new Value(Value.Kind.IDENTIFIER, "b", 8)
                    ),
                    new Span("foo a:x b", 1)
                ),
                "a bound arg followed by an unbound one was accepted"
            ).pos(),
            Matchers.equalTo(8)
        );
    }

    @Test
    void rejectsUnboundFollowedByBound() {
        Assertions.assertThrows(
            ParseError.class,
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, "y")
                ),
                new Span("foo a b:y", 1)
            ),
            "an unbound arg followed by a bound one was accepted"
        );
    }

    @Test
    void reportsErrorAtFirstDivergentArg() {
        MatcherAssert.assertThat(
            "the error must point at the column of the first divergent arg",
            Assertions.assertThrows(
                ParseError.class,
                () -> Bindings.checkAllOrNothing(
                    Arrays.asList(
                        new Value(Value.Kind.IDENTIFIER, "a", 4),
                        new Value(Value.Kind.IDENTIFIER, "b", 6),
                        new Value(Value.Kind.IDENTIFIER, "c", 8, "z")
                    ),
                    new Span("foo a b c:z", 1)
                ),
                "the divergent arg's column was not the one reported"
            ).pos(),
            Matchers.equalTo(8)
        );
    }

    @Test
    void acceptsReceiverWithoutBinding() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkReceiver(
                new Value(Value.Kind.IDENTIFIER, "cond", 4),
                new Span("if. cond then else", 1)
            ),
            "a bare receiver was rejected"
        );
    }

    @Test
    void rejectsReceiverWithBinding() {
        Assertions.assertThrows(
            ParseError.class,
            () -> Bindings.checkReceiver(
                new Value(Value.Kind.IDENTIFIER, "cond", 4, "x"),
                new Span("if. cond:x then else", 1)
            ),
            "a receiver carrying a binding was accepted"
        );
    }

    @Test
    void rejectsBindingOnAChildOfTheBottomSentinel() {
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.HEAD, Openness.OPEN);
        Assertions.assertThrows(
            ParseError.class,
            () -> Bindings.observeChild(stack, "äß", new Span("foo", 1)),
            "a binding on a top-level line was accepted"
        );
    }

    @Test
    void acceptsAnUpgradeAgainstTheBottomSentinel() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkReceiverUpgrade(
                new Stack().below(), new Span(".plus 1 > x", 1)
            ),
            "a chain continuation with no parent below was rejected"
        );
    }
}
