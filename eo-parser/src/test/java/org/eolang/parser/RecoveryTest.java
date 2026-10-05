/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Recovery}.
 *
 * @since 0.1
 */
final class RecoveryTest {

    @Test
    void resumesAtSiblingOfFailedLine() {
        MatcherAssert.assertThat(
            "the walk must resume at the first line back at the indent of the failed one",
            new Recovery(
                Arrays.asList(
                    new Span("  über! ☃", 1),
                    new Span("    क्ष", 2),
                    new Span("      deeper", 3),
                    new Span("  sibling", 4)
                )
            ).after(0),
            Matchers.equalTo(3)
        );
    }

    @Test
    void resumesAtBlankSeparatingSibling() {
        MatcherAssert.assertThat(
            "the blank separating the failed block from the next object must be handed back",
            new Recovery(
                Arrays.asList(
                    new Span("  bad!!!", 1),
                    new Span("    child", 2),
                    new Span("", 3),
                    new Span("next", 4)
                )
            ).after(0),
            Matchers.equalTo(2)
        );
    }

    @Test
    void skipsBlanksTrailingBlockAtEndOfFile() {
        MatcherAssert.assertThat(
            "blanks with no object left below them separate nothing and stay skipped",
            new Recovery(
                Arrays.asList(
                    new Span("bad!!!", 1),
                    new Span("  child", 2),
                    new Span("", 3)
                )
            ).after(0),
            Matchers.equalTo(3)
        );
    }

    @Test
    void skipsBlankLineInsideBlock() {
        MatcherAssert.assertThat(
            "a blank line inside the block of a failed line must not resume the walk",
            new Recovery(
                Arrays.asList(
                    new Span("bad!!!", 1),
                    new Span("  child", 2),
                    new Span("", 3),
                    new Span("  child", 4),
                    new Span("next", 5)
                )
            ).after(0),
            Matchers.equalTo(4)
        );
    }

    @Test
    void skipsFromAnIndexOtherThanTheFailedLineItself() {
        MatcherAssert.assertThat(
            "skip must scan from the given index using the given indent, "
                .concat("not the indent of the line at that index"),
            new Recovery(
                Arrays.asList(
                    new Span("01-02-", 1),
                    new Span("03-04", 2),
                    new Span("  child", 3),
                    new Span("sibling", 4)
                )
            ).skip(2, 0),
            Matchers.equalTo(3)
        );
    }
}
