/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EObytes}.
 *
 * @since 0.23
 */
final class EObytesEOsliceTest {

    @Test
    void takesLegalSlice() {
        MatcherAssert.assertThat(
            "slice is taken correctly",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new Data.ToPhi("hello, world!")
                            .take("as-bytes")
                            .take("slice")
                            .copy(),
                        "start",
                        new Data.ToPhi(2)
                    ),
                    "len",
                    new Data.ToPhi(5)
                )
            ).asString(),
            Matchers.equalTo("llo, ")
        );
    }

    @Test
    void throwsOnOutOfBoundsSlice() {
        MatcherAssert.assertThat(
            "an out-of-bounds slice with no fallback must terminate with the reason",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        ExAbstract.class,
                        () -> new Dataized(
                            new PhApplication(
                                new PhApplication(
                                    new Data.ToPhi("hello")
                                        .take("as-bytes")
                                        .take("slice")
                                        .copy(),
                                    "start",
                                    new Data.ToPhi(3)
                                ),
                                "len",
                                new Data.ToPhi(10)
                            )
                        ).asString(),
                        "doesnt terminate on out of bounds slice"
                    )
                )
            ).asString(),
            Matchers.containsString("bytes of size 5")
        );
    }

    @Test
    void takesWrongSlice() {
        MatcherAssert.assertThat(
            "error message is correct",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        ExFailure.class,
                        () -> new Dataized(
                            new PhApplication(
                                new PhApplication(
                                    new Data.ToPhi("hello, world!")
                                        .take("as-bytes")
                                        .take("slice")
                                        .copy(),
                                    "start",
                                    new Data.ToPhi(2)
                                ),
                                "len",
                                new Data.ToPhi(-5)
                            )
                        ).asString(),
                        "fails on check"
                    )
                )
            ).asString(),
            Matchers.containsString("the 'len' attribute (-5) must be greater or equal to zero")
        );
    }

    @Test
    void reportsRealOffsetWhenItExceedsIntRange() {
        MatcherAssert.assertThat(
            "an offset above Integer.MAX_VALUE must be rejected with the real value, not silently saturated",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        RuntimeException.class,
                        () -> new Dataized(
                            new PhApplication(
                                new PhApplication(
                                    new Data.ToPhi("hello, world!")
                                        .take("as-bytes")
                                        .take("slice")
                                        .copy(),
                                    "start",
                                    new Data.ToPhi(3.0e9)
                                ),
                                "len",
                                new Data.ToPhi(1)
                            )
                        ).asString(),
                        "doesnt reject an offset above int range"
                    )
                )
            ).asString(),
            Matchers.containsString("must fit into int range")
        );
    }
}
