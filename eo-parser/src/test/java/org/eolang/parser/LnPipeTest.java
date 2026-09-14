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
 * Test case for {@link LnPipe}.
 *
 * @since 0.1
 */
final class LnPipeTest {

    @Test
    void rejectsPlusGreaterSuffix() {
        final Stack stack = new Stack();
        new LnFormation(new Span("[] > foo", 1)).into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a pipe carrying a `+>` suffix must be rejected instead of silently accepted",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnPipe(new Span("| +> x", 2))
                    .into(stack, new Globals(), new Emit())
            ).getMessage(),
            Matchers.equalTo("a pipe application cannot declare a test attribute")
        );
    }

    @Test
    void rejectsPipeUnderAtomFormation() {
        final Stack stack = new Stack();
        new LnFormation(new Span("[] > foo /number", 1)).into(stack, new Globals(), new Emit());
        new LnFormation(new Span("  [] +> can-x", 2)).into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a pipe replacing the top under an atom-bodied formation must be rejected,"
                .concat(" proving the Admission built for it is never permitted"),
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnPipe(new Span("  | > x", 3))
                    .into(stack, new Globals(), new Emit())
            ).getMessage(),
            Matchers.equalTo("atom may contain only test attributes")
        );
    }

    @Test
    void acceptsPipeUnderNonAtomFormation() {
        final Stack stack = new Stack();
        new LnFormation(new Span("[] > foo", 1)).into(stack, new Globals(), new Emit());
        new LnFormation(new Span("  [] > bar", 2)).into(stack, new Globals(), new Emit());
        Assertions.assertDoesNotThrow(
            () -> new LnPipe(new Span("  | > x", 3))
                .into(stack, new Globals(), new Emit()),
            "a pipe replacing a named formation's top under a non-atom parent must be accepted"
        );
    }
}
