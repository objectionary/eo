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
 * raised by {@link EOmalloc$EOof$EOallocated$EOsize},
 * {@link EOmalloc$EOof$EOallocated$EOread}, and
 * {@link EOmalloc$EOof$EOallocated$EOwrite} when their
 * integer attributes are invalid.
 * @since 0.51
 * @checkstyle TypeNameCheck (5 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOmallocAllocatedExpectTest {

    @Test
    void throwsCorrectErrorForNegativeIdInSize() {
        new EOmallocAllocatedExpectTest.Throws(
            new PhWith(
                new EOmalloc$EOof$EOallocated$EOsize(),
                Phi.RHO,
                new PhWith(
                    new EOmallocAllocatedExpectTest.Dummy(),
                    "id",
                    new Data.ToPhi(-42)
                )
            ),
            "size with negative id must fail with a proper message",
            "the 'id' attribute (-42) must be greater or equal to zero"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForNonNumericIdInRead() {
        new EOmallocAllocatedExpectTest.Throws(
            new PhWith(
                new EOmalloc$EOof$EOallocated$EOread(),
                Phi.RHO,
                new PhWith(
                    new EOmallocAllocatedExpectTest.Dummy(),
                    "id",
                    new Data.ToPhi(true)
                )
            ),
            "read with non-numeric id must fail with a proper message",
            "the 'id' attribute must be a number"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForFractionalOffsetInRead() {
        new EOmallocAllocatedExpectTest.Throws(
            new EOmallocAllocatedExpectTest.Read(
                new Data.ToPhi(1.5), new Data.ToPhi(0)
            ).it(),
            "read with fractional offset must fail with a proper message",
            "the 'offset' attribute (1.5) must be an integer"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForNegativeLengthInRead() {
        new EOmallocAllocatedExpectTest.Throws(
            new EOmallocAllocatedExpectTest.Read(
                new Data.ToPhi(0), new Data.ToPhi(-1)
            ).it(),
            "read with negative length must fail with a proper message",
            "the 'length' attribute (-1) must be greater or equal to zero"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForNonNumericIdInWrite() {
        new EOmallocAllocatedExpectTest.Throws(
            new EOmallocAllocatedExpectTest.Write(
                new Data.ToPhi(true), new Data.ToPhi(0)
            ).it(),
            "write with non-numeric id must fail with a proper message",
            "the 'id' attribute must be a number"
        ).verify();
    }

    @Test
    void throwsCorrectErrorForFractionalOffsetInWrite() {
        new EOmallocAllocatedExpectTest.Throws(
            new EOmallocAllocatedExpectTest.Write(
                new Data.ToPhi(0), new Data.ToPhi(1.5)
            ).it(),
            "write with fractional offset must fail with a proper message",
            "the 'offset' attribute (1.5) must be an integer"
        ).verify();
    }

    /**
     * Minimal Phi with a single {@code id} attribute used as a stand-in
     * for the {@code allocated} parent in tests.
     * @since 0.51
     */
    private static final class Dummy extends PhDefault {

        Dummy() {
            super(new Attrs(new AttrEntry("id", new AtVoid("id"))));
        }
    }

    /**
     * A {@code malloc.of.allocated.read} Phi with a valid id and the
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
            return new PhWith(
                new PhWith(
                    new PhWith(
                        new EOmalloc$EOof$EOallocated$EOread(),
                        Phi.RHO,
                        new PhWith(
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

    /**
     * A {@code malloc.of.allocated.write} Phi with a configurable id,
     * configurable {@code offset}, and a fixed {@code data} payload.
     * @since 0.51
     */
    private static final class Write {

        /**
         * Id attribute value placed on the rho stand-in.
         */
        private final Phi id;

        /**
         * Offset attribute value.
         */
        private final Phi offset;

        Write(final Phi id, final Phi offset) {
            this.id = id;
            this.offset = offset;
        }

        /**
         * Return it.
         * @return The configured write Phi
         * @checkstyle MethodNameCheck (5 lines)
         */
        Phi it() {
            return new PhWith(
                new PhWith(
                    new PhWith(
                        new EOmalloc$EOof$EOallocated$EOwrite(),
                        Phi.RHO,
                        new PhWith(
                            new EOmallocAllocatedExpectTest.Dummy(),
                            "id",
                            this.id
                        )
                    ),
                    "offset",
                    this.offset
                ),
                "data",
                new Data.ToPhi(0)
            );
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
