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
 * raised by {@link EOchunk$EOwrite} when its integer attributes
 * are invalid.
 * @since 0.51
 */
final class EOchunkEOwriteTest {

    @Test
    void throwsCorrectErrorForNonNumericId() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOchunk$EOwrite(),
                        Phi.RHO,
                        new PhApplication(
                            new PhDefault(new Attrs(new Attr("id", new AtVoid("id")))),
                            "id",
                            new Data.ToPhi(true)
                        )
                    )
                ).take(),
                "write with non-numeric id must fail with a proper message"
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
                    new PhApplication(
                        new PhApplication(
                            new EOchunk$EOwrite(),
                            Phi.RHO,
                            new PhApplication(
                                new PhDefault(new Attrs(new Attr("id", new AtVoid("id")))),
                                "id",
                                new Data.ToPhi(0)
                            )
                        ),
                        "offset",
                        new Data.ToPhi(1.5)
                    )
                ).take(),
                "write with fractional offset must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute (1.5) must be an integer")
        );
    }
}
