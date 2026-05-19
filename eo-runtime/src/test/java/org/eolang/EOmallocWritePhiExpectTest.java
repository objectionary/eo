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
 * raised by {@link EOmalloc$EOof$EOallocated$EOwrite} and
 * the {@code malloc.of.@} atom when their integer attributes
 * are invalid.
 * @since 0.51
 * @checkstyle TypeNameCheck (5 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOmallocWritePhiExpectTest {

    @Test
    void throwsCorrectErrorForNonNumericIdInWrite() {
        new EOmallocWritePhiExpectTest.Throws(
            new PhWith(
                new EOmalloc$EOof$EOallocated$EOwrite(),
                Phi.RHO,
                new PhWith(
                    new EOmallocWritePhiExpectTest.Dummy(),
                    "id",
                    new Data.ToPhi(true)
                )
            ),
            "write with non-numeric id must fail with a proper message",
            "the 'id' attribute must be a number"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForFractionalOffsetInWrite() {
        new EOmallocWritePhiExpectTest.Throws(
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
            ),
            "write with fractional offset must fail with a proper message",
            "the 'offset' attribute (1.5) must be an integer"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForNonNumericSize() {
        new EOmallocWritePhiExpectTest.Throws(
            new PhWith(
                new EOmalloc$EOof$EOφ(),
                Phi.RHO,
                new PhWith(
                    new EOmallocWritePhiExpectTest.SizedDummy(),
                    "size",
                    new Data.ToPhi(true)
                )
            ),
            "malloc.of.@ with non-numeric size must fail with a proper message",
            "the 'size' attribute must be a number"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForNegativeSize() {
        new EOmallocWritePhiExpectTest.Throws(
            new PhWith(
                new EOmalloc$EOof$EOφ(),
                Phi.RHO,
                new PhWith(
                    new EOmallocWritePhiExpectTest.SizedDummy(),
                    "size",
                    new Data.ToPhi(-1)
                )
            ),
            "malloc.of.@ with negative size must fail with a proper message",
            "the 'size' attribute (-1) must be greater or equal to zero"
        ).verify();
    }

    /**
     * Minimal Phi with a single {@code id} attribute used as a stand-in
     * for the {@code allocated} parent in write tests.
     * @since 0.51
     */
    private static final class Dummy extends PhDefault {

        Dummy() {
            super(new Attrs(new AttrEntry("id", new AtVoid("id"))));
        }
    }

    /**
     * Minimal Phi with a single {@code size} attribute used as a stand-in
     * for the {@code malloc.of} parent in {@code malloc.of.@} tests.
     * @since 0.51
     */
    private static final class SizedDummy extends PhDefault {

        SizedDummy() {
            super(new Attrs(new AttrEntry("size", new AtVoid("size"))));
        }
    }

    /**
     * Asserts that dataizing the given Phi throws {@link ExAbstract} with
     * the expected error message.
     * @since 0.51
     */
    private static final class Throws {

        /**
         * Phi expected to throw on dataization.
         */
        private final Phi phi;

        /**
         * Reason passed to {@code assertThrows} for the scenario.
         */
        private final String scenario;

        /**
         * Expected exception message.
         */
        private final String expected;

        Throws(final Phi phi, final String scenario, final String expected) {
            this.phi = phi;
            this.scenario = scenario;
            this.expected = expected;
        }

        /**
         * Run the assertion.
         */
        void verify() {
            MatcherAssert.assertThat(
                "the message in the error is correct",
                Assertions.assertThrows(
                    ExAbstract.class,
                    () -> new Dataized(this.phi).take(),
                    this.scenario
                ).getMessage(),
                Matchers.equalTo(this.expected)
            );
        }
    }
}
