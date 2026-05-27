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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case verifying {@link Expect}-based error messages
 * raised by {@link EObytes$EOright} when the {@code x} attribute
 * is not an integer.
 * @since 0.51
 * @checkstyle TypeNameCheck (5 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EObytesEOrightExpectTest {

    @Test
    void throwsCorrectErrorForNonNumericX() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            new EObytes$EOright(),
                            Phi.RHO,
                            new Data.ToPhi(new byte[]{0x01, 0x02})
                        ),
                        "x",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "right with non-numeric x must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'x' attribute must be a number")
        );
    }

    @Test
    void throwsCorrectErrorForFractionalX() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            new EObytes$EOright(),
                            Phi.RHO,
                            new Data.ToPhi(new byte[]{0x01, 0x02})
                        ),
                        "x",
                        new Data.ToPhi(1.5)
                    )
                ).take(),
                "right with fractional x must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'x' attribute (1.5) must be an integer")
        );
    }
}
