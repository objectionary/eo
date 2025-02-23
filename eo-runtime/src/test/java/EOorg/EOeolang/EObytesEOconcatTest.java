/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtCompositeTest;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EObytes}.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (4 lines)
 */
final class EObytesEOconcatTest {

    @Test
    void concatenatesBytes() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new PhWith(
                    Phi.Φ.take("org.eolang.string"),
                    0,
                    new PhWith(
                        new Data.ToPhi("привет ").take("as-bytes").take("concat").copy(),
                        "b",
                        new Data.ToPhi("mr. ㄤㄠ!").take("as-bytes")
                    )
                )
            ).asString(),
            Matchers.equalTo("привет mr. ㄤㄠ!")
        );
    }

}
