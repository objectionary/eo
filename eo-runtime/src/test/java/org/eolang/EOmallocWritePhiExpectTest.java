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
 * raised by {@link EOmalloc$EOof$EOallocated$EOwrite} and
 * the {@code malloc.of.@} atom when their integer attributes
 * are invalid.
 * @since 0.51
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOmallocWritePhiExpectTest {

    @Test
    void throwsCorrectErrorForNonNumericIdInWrite() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new EOmalloc$EOof$EOallocated$EOwrite(),
                        Phi.RHO,
                        new PhWith(
                            new EOmallocWritePhiExpectTest.Dummy(),
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
    void throwsCorrectErrorForFractionalOffsetInWrite() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhWith(
                            new EOmalloc$EOof$EOallocated$EOwrite(),
                            Phi.RHO,
                            new PhWith(
                                new EOmallocWritePhiExpectTest.Dummy(),
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

    @Test
    void throwsCorrectErrorForNonNumericSize() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new EOmalloc$EOof$EOφ(),
                        Phi.RHO,
                        new PhWith(
                            new EOmallocWritePhiExpectTest.Dummy(),
                            "size",
                            new Data.ToPhi(true)
                        )
                    )
                ).take(),
                "malloc.of.@ with non-numeric size must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'size' attribute must be a number")
        );
    }

    @Test
    void throwsCorrectErrorForNegativeSize() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new EOmalloc$EOof$EOφ(),
                        Phi.RHO,
                        new PhWith(
                            new EOmallocWritePhiExpectTest.Dummy(),
                            "size",
                            new Data.ToPhi(-1)
                        )
                    )
                ).take(),
                "malloc.of.@ with negative size must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo("the 'size' attribute (-1) must be greater or equal to zero")
        );
    }

    /**
     * Minimal Phi with {@code id} and {@code size} attributes,
     * used as a stand-in for the {@code allocated} parent in write
     * tests and the {@code malloc.of} parent in {@code malloc.of.@} tests.
     * @since 0.51
     */
    private static final class Dummy extends PhDefault {

        Dummy() {
            super(new Attrs(
                new AttrEntry("id", new AtVoid("id")),
                new AttrEntry("size", new AtVoid("size"))
            ));
        }
    }
}
