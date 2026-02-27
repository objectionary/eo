/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhWith;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EObytes}.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (4 lines)
 */
final class EObytesEOsliceTest {

    @Test
    void takesLegalSlice() {
        MatcherAssert.assertThat(
            "slice is taken correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
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
    @SuppressWarnings("PMD.UnitTestContainsTooManyAsserts")
    void takesWrongSlice() {
        MatcherAssert.assertThat(
            "error message is correct",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        EOerror.ExError.class,
                        () -> new Dataized(
                            new PhWith(
                                new PhWith(
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
            Matchers.containsString("the 'len' attribute (-5) must be a positive integer")
        );
    }

}
