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
 * raised by {@link EObytes$EOleft} when the {@code b} attribute
 * is not a shift count.
 *
 * @since 0.78.0
 */
final class EObytesEOleftTest {

    @Test
    void throwsCorrectErrorForNonNumericB() {
        MatcherAssert.assertThat(
            "the message in the error is not correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            new EObytes$EOleft(),
                            Phi.RHO,
                            new Data.ToPhi(new byte[]{0x01, 0x02})
                        ),
                        "b",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "left with non-numeric b cannot pass"
            ).getMessage(),
            Matchers.equalTo("the 'b' attribute must be a number")
        );
    }

    @Test
    void throwsCorrectErrorForFractionalB() {
        MatcherAssert.assertThat(
            "the message in the error is not correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            new EObytes$EOleft(),
                            Phi.RHO,
                            new Data.ToPhi(new byte[]{0x01, 0x02})
                        ),
                        "b",
                        new Data.ToPhi(1.5)
                    )
                ).take(),
                "left with fractional b cannot pass"
            ).getMessage(),
            Matchers.equalTo("the 'b' attribute (1.5) must be an integer")
        );
    }

    @Test
    void throwsCorrectErrorForTooLargeB() {
        MatcherAssert.assertThat(
            "the message in the error is not correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            new EObytes$EOleft(),
                            Phi.RHO,
                            new Data.ToPhi(new byte[]{0x01, 0x02})
                        ),
                        "b",
                        new Data.ToPhi(2_147_483_648.0)
                    )
                ).take(),
                "left with b above the int range cannot pass"
            ).getMessage(),
            Matchers.equalTo("the 'b' attribute (2.147483648E9) must fit into int range")
        );
    }
}
