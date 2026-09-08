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
 * raised by {@link EOchunk$EOsize} when its {@code id} attribute
 * is invalid.
 *
 * @since 0.51
 */
final class EOchunkEOsizeTest {

    @Test
    void throwsCorrectErrorForNegativeId() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOchunk$EOsize(),
                        Phi.RHO,
                        new PhApplication(
                            new PhDefault(new Attrs(new Attr("id", new AtVoid("id")))),
                            "id",
                            new Data.ToPhi(-42)
                        )
                    )
                ).take(),
                "size with negative id must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'id' attribute (-42) must be greater or equal to zero")
        );
    }
}
