/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.eolang.Bind;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@code string.printf}.
 * @since 0.73.3
 */
final class EOprintfTest {

    @ParameterizedTest
    @ValueSource(doubles = {9.3e18, 1.0e19, -1.0e19})
    void reportsOutOfLongRangeReasonForDecimalConversion(final double number) {
        MatcherAssert.assertThat(
            String.format(
                "the number %s is not reported as out of the long range, but as something else",
                number
            ),
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        Phi.Φ.take("string.printf").copy(),
                        new Bind("format", new Data.ToPhi("%d")),
                        new Bind("args", new Data.ToPhi(new Phi[]{new Data.ToPhi(number)}))
                    )
                ).take()
            ).toString(),
            Matchers.containsString("doesn't fit into the long range")
        );
    }
}
