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
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOonError}.
 * @since 0.57
 */
final class EOonErrorTest {

    @Test
    void catchesException() {
        MatcherAssert.assertThat(
            "catches exception",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new PhApplication(
                            new EOonError(),
                            0, new PhSafe(
                                new EOonErrorTest.Broken(
                                    EOonErrorTest.error("it is broken")
                                )
                            )
                        ),
                        1, new EOonErrorTest.Handler()
                    ),
                    2,
                    new EOonErrorTest.FinallyOk()
                )
            ).asString(),
            Matchers.containsString("it is brok")
        );
    }

    /**
     * Build error enclosure without {@link Data.ToPhi}.
     * @param text Error text
     * @return Error object
     */
    private static Phi error(final String text) {
        final Phi enc = new EOerror().copy();
        enc.put(
            "message",
            new PhDefault(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        );
        return enc;
    }

    /**
     * Broken.
     * @since 0.57
     */
    private static final class Broken extends PhDefault implements Atom {

        /**
         * Error enclosure.
         */
        private final Phi error;

        /**
         * Ctor.
         * @param err Error enclosure
         */
        Broken(final Phi err) {
            super();
            this.error = err;
        }

        @Override
        public Phi lambda() {
            throw new EOerror.ExError(this.error);
        }
    }

    /**
     * Finally block that dataizes without touching phi.
     * @since 0.57
     */
    private static final class FinallyOk extends PhDefault implements Atom {

        /**
         * Ctor.
         */
        FinallyOk() {
            super();
        }

        @Override
        public Phi lambda() {
            return new PhDefault(new byte[] {(byte) 0x01});
        }
    }

    /**
     * Handler.
     * @since 0.57
     */
    private static final class Handler extends PhDefault implements Atom {

        /**
         * Ctor.
         */
        Handler() {
            super(new Attrs(new Attr("ex", new AtVoid("ex"))));
        }

        @Override
        public Phi lambda() {
            return this.take("ex").take("message");
        }
    }
}
