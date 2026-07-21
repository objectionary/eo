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
 * raised by {@link EOchunk$EOsize} and
 * {@link EOchunk$EOread} when their integer
 * attributes are invalid.
 * @since 0.51
 * @checkstyle TypeNameCheck (5 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOmallocAllocatedExpectTest {

    @Test
    void throwsCorrectErrorForNegativeIdInSize() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOchunk$EOsize(),
                        Phi.RHO,
                        new PhApplication(
                            new EOmallocAllocatedExpectTest.Dummy(),
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

    @Test
    void throwsCorrectErrorForNonNumericIdInRead() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOchunk$EOread(),
                        Phi.RHO,
                        new PhApplication(
                            new EOmallocAllocatedExpectTest.Dummy(),
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
    void throwsCorrectErrorForFractionalOffsetInRead() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new EOmallocAllocatedExpectTest.Read(
                        new Data.ToPhi(1.5), new Data.ToPhi(0)
                    ).it()
                ).take(),
                "read with fractional offset must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute (1.5) must be an integer")
        );
    }

    @Test
    void throwsCorrectErrorForNegativeLengthInRead() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new EOmallocAllocatedExpectTest.Read(
                        new Data.ToPhi(0), new Data.ToPhi(-1)
                    ).it()
                ).take(),
                "read with negative length must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'length' attribute (-1) must be greater or equal to zero")
        );
    }

    /**
     * Minimal Phi with a single {@code id} attribute used as a stand-in
     * for the {@code allocated} parent in tests.
     * @since 0.51
     */
    private static final class Dummy extends PhDefault {

        Dummy() {
            super(new Attrs(new Attr("id", new AtVoid("id"))));
        }
    }

    /**
     * A {@code chunk.read} Phi with a valid id and the
     * given {@code offset} and {@code length}.
     * @since 0.51
     */
    private static final class Read {

        /**
         * Offset attribute value.
         */
        private final Phi offset;

        /**
         * Length attribute value.
         */
        private final Phi length;

        Read(final Phi offset, final Phi length) {
            this.offset = offset;
            this.length = length;
        }

        /**
         * Return it.
         * @return The configured read Phi
         * @checkstyle MethodNameCheck (5 lines)
         */
        Phi it() {
            return new PhApplication(
                new PhApplication(
                    new PhApplication(
                        new EOchunk$EOread(),
                        Phi.RHO,
                        new PhApplication(
                            new EOmallocAllocatedExpectTest.Dummy(),
                            "id",
                            new Data.ToPhi(0)
                        )
                    ),
                    "offset",
                    this.offset
                ),
                "length",
                this.length
            );
        }
    }
}
