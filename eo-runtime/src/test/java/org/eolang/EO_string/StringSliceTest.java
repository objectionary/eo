/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@code string.slice} out-of-bounds behavior.
 * @since 0.57.4
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class StringSliceTest {

    @Test
    void takesLegalSlice() {
        MatcherAssert.assertThat(
            "a valid slice is taken correctly",
            new Dataized(
                StringSliceTest.slice("hello", 2, 3)
            ).asString(),
            Matchers.equalTo("llo")
        );
    }

    @Test
    void surfacesOutOfBoundsCauseForLength() {
        MatcherAssert.assertThat(
            "the real out-of-bounds cause must reach the caller, not the 'must be a number' mask",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        ExAbstract.class,
                        () -> new Dataized(
                            StringSliceTest.slice("hello", 2, 10)
                        ).asString(),
                        "an out-of-range length must terminate with the reason"
                    )
                )
            ).asString(),
            Matchers.containsString("out of string bounds")
        );
    }

    @Test
    void surfacesOutOfBoundsCauseForStart() {
        MatcherAssert.assertThat(
            "the real out-of-bounds start cause must reach the caller, not the 'must be a number' mask",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        ExAbstract.class,
                        () -> new Dataized(
                            StringSliceTest.slice("hello", 10, 1)
                        ).asString(),
                        "an out-of-range start must terminate with the reason"
                    )
                )
            ).asString(),
            Matchers.containsString("Start index (10) is out of string bounds")
        );
    }

    @Test
    void throwsWhenStartPastEndWithZeroLength() {
        MatcherAssert.assertThat(
            "an out-of-range start must be rejected even when the length is zero",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        ExAbstract.class,
                        () -> new Dataized(
                            StringSliceTest.slice("hello", 10, 0)
                        ).asString(),
                        "a zero length must not silently ignore an out-of-range start"
                    )
                )
            ).asString(),
            Matchers.containsString("Start index (10) is out of string bounds")
        );
    }

    /**
     * Build {@code text.slice(start, len)} as a {@link Phi}.
     * @param text The string to slice
     * @param start The start index
     * @param len The length to slice
     * @return The slice application
     */
    private static Phi slice(final String text, final int start, final int len) {
        return new PhApplication(
            new PhApplication(
                new Data.ToPhi(text).take("slice").copy(),
                "start",
                new Data.ToPhi(start)
            ),
            "len",
            new Data.ToPhi(len)
        );
    }
}
