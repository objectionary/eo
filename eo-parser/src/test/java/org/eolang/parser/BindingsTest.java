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
 * @since 0.1
 */
final class BindingsTest {

    @Test
    void acceptsEmptyArgs() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Collections.emptyList(), new Span("foo", 1)
            ),
            "an empty arg list must pass the all-or-nothing rule trivially"
        );
    }

    @Test
    void acceptsSingleArg() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Collections.singletonList(new Value(Value.Kind.IDENTIFIER, "a", 4, 5)),
                new Span("foo a", 1)
            ),
            "a single arg cannot violate the all-or-nothing rule"
        );
    }

    @Test
    void acceptsAllUnboundArgs() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, 5),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, 7),
                    new Value(Value.Kind.IDENTIFIER, "c", 8, 9)
                ),
                new Span("foo a b c", 1)
            ),
            "all-unbound is a valid uniform mode"
        );
    }

    @Test
    void acceptsAllBoundArgs() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, 5, "x"),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, 7, "y")
                ),
                new Span("foo a:x b:y", 1)
            ),
            "all-bound is a valid uniform mode"
        );
    }

    @Test
    void acceptsAllBoundArgsWithNumericBinding() {
        final long seed = System.nanoTime();
        final int slot = new Random(seed).nextInt(10);
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, 5, Integer.toString(slot)),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, 7, Integer.toString(slot))
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
                    new Value(Value.Kind.IDENTIFIER, "a", 4, 5, ""),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, 7, "")
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
                        new Value(Value.Kind.IDENTIFIER, "a", 4, 5, ""),
                        new Value(Value.Kind.IDENTIFIER, "b", 6, 7)
                    ),
                    new Span("foo a: b", 1)
                )
            ).pos(),
            Matchers.equalTo(6)
        );
    }

    @Test
    void rejectsMixedBoundAndUnbound() {
        Assertions.assertThrows(
            ParseError.class,
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, 5, "x"),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, 7)
                ),
                new Span("foo a:x b", 1)
            ),
            "a bound arg followed by an unbound one must be rejected per R-6.6.2"
        );
    }

    @Test
    void rejectsUnboundFollowedByBound() {
        Assertions.assertThrows(
            ParseError.class,
            () -> Bindings.checkAllOrNothing(
                Arrays.asList(
                    new Value(Value.Kind.IDENTIFIER, "a", 4, 5),
                    new Value(Value.Kind.IDENTIFIER, "b", 6, 7, "y")
                ),
                new Span("foo a b:y", 1)
            ),
            "an unbound arg followed by a bound one must be rejected per R-6.6.2"
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
                        new Value(Value.Kind.IDENTIFIER, "a", 4, 5),
                        new Value(Value.Kind.IDENTIFIER, "b", 6, 7),
                        new Value(Value.Kind.IDENTIFIER, "c", 8, 9, "z")
                    ),
                    new Span("foo a b c:z", 1)
                ),
                "the divergent arg's column must be reported"
            ).pos(),
            Matchers.equalTo(8)
        );
    }

    @Test
    void acceptsReceiverWithoutBinding() {
        Assertions.assertDoesNotThrow(
            () -> Bindings.checkReceiver(
                new Value(Value.Kind.IDENTIFIER, "cond", 4, 8),
                new Span("if. cond then else", 1)
            ),
            "a bare receiver is the canonical form for reversed dispatch"
        );
    }

    @Test
    void rejectsReceiverWithBinding() {
        Assertions.assertThrows(
            ParseError.class,
            () -> Bindings.checkReceiver(
                new Value(Value.Kind.IDENTIFIER, "cond", 4, 8, "x"),
                new Span("if. cond:x then else", 1)
            ),
            "a receiver carrying a binding must be rejected per R-6.6.3"
        );
    }
}
