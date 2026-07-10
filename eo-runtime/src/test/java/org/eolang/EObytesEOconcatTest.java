/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EObytes}.
 * @since 0.23
 * @checkstyle TypeNameCheck (4 lines)
 */
final class EObytesEOconcatTest {

    @Test
    void concatenatesBytes() {
        MatcherAssert.assertThat(
            "Concatenation of byte arrays should produce 'привет mr. ㄤㄠ!', but it didn't",
            new Dataized(
                new PhApplication(
                    Phi.Φ.take("string"),
                    0,
                    new PhApplication(
                        new Data.ToPhi("привет ").take("as-bytes").take("concat").copy(),
                        "b",
                        new Data.ToPhi("mr. ㄤㄠ!").take("as-bytes")
                    )
                )
            ).asString(),
            Matchers.equalTo("привет mr. ㄤㄠ!")
        );
    }

    @Test
    void reportsCleanFailureInsteadOfOverflowingArraySize() {
        MatcherAssert.assertThat(
            "Concatenation overflowing the maximum array size should report a clean failure via the 'cant-concat' fallback, but it didn't",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new Data.ToPhi(new byte[1_073_741_824]).take("concat").copy(),
                        "b",
                        new Data.ToPhi(new byte[1_073_741_824])
                    ),
                    "cant-concat",
                    new EObytesEOconcatTest.Fallback()
                )
            ).asString(),
            Matchers.equalTo("recovered")
        );
    }

    /**
     * Fallback object, mostly for unit tests.
     * @since 0.74
     */
    private static final class Fallback extends PhDefault {

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (5 lines)
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Fallback() {
            this.add("msg", new AtVoid("msg"));
            this.add("φ", new AtComposite(this, rho -> new Data.ToPhi("recovered")));
        }
    }
}
