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
 * raised by {@link EOchunk$EOread} when its integer attributes
 * are invalid.
 *
 * @since 0.51
 */
final class EOchunkEOreadTest {

    @Test
    void throwsCorrectErrorForNonNumericId() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOchunk$EOread(),
                        Phi.RHO,
                        new PhApplication(
                            new PhDefault(new Attrs(new Attr("id", new AtVoid("id")))),
                            "id",
                            new Data.ToPhi(true)
                        )
                    )
                ).take(),
                "read with non-numeric id must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'id' attribute must be a number")
        );
    }

    @Test
    void throwsCorrectErrorForFractionalOffset() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    EOchunkEOreadTest.read(
                        new Data.ToPhi(1.5), new Data.ToPhi(0)
                    )
                ).take(),
                "read with fractional offset must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute (1.5) must be an integer")
        );
    }

    @Test
    void throwsCorrectErrorForNegativeLength() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    EOchunkEOreadTest.read(
                        new Data.ToPhi(0), new Data.ToPhi(-1)
                    )
                ).take(),
                "read with negative length must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'length' attribute (-1) must be greater or equal to zero")
        );
    }

    private static Phi read(final Phi offset, final Phi length) {
        return new PhApplication(
            new PhApplication(
                new PhApplication(
                    new EOchunk$EOread(),
                    Phi.RHO,
                    new PhApplication(
                        new PhDefault(new Attrs(new Attr("id", new AtVoid("id")))),
                        "id",
                        new Data.ToPhi(0)
                    )
                ),
                "offset",
                offset
            ),
            "length",
            length
        );
    }
}
