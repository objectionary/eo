/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case verifying {@link Expect}-based error messages
 * raised by {@link EObytes$EOright} when the {@code b} attribute
 * is not an integer.
 *
 * @since 0.51
 */
final class EObytesEOrightTest {

    @Test
    void throwsCorrectErrorForNonNumericB() {
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
                        "b",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "right with non-numeric b must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'b' attribute must be a number")
        );
    }

    @Test
    void throwsCorrectErrorForFractionalB() {
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
                        "b",
                        new Data.ToPhi(1.5)
                    )
                ).take(),
                "right with fractional b must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'b' attribute (1.5) must be an integer")
        );
    }
}
