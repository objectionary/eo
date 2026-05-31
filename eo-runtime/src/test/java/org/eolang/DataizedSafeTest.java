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
 * Tests for {@link DataizedSafe}.
 * @since 0.57
 */
final class DataizedSafeTest {

    @Test
    void takesAttributeOnSuccess() {
        MatcherAssert.assertThat(
            "must return attribute value",
            new Dataized(
                DataizedSafe.take(
                    new DataizedSafeTest.Ok(new PhDefault(new BytesOf(7.0).take())),
                    "x"
                )
            ).asNumber(),
            Matchers.equalTo(7.0)
        );
    }

    @Test
    void returnsErrorOnFailure() {
        MatcherAssert.assertThat(
            "must return error enclosure",
            DataizedSafe.take(
                new DataizedSafeTest.Broken(DataizedSafeTest.error("it is broken")),
                "x"
            ).forma(),
            Matchers.containsString("error")
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
     * Ok object.
     * @since 0.57
     */
    private static final class Ok extends PhDefault {

        /**
         * Ctor.
         * @param value Attribute value
         */
        Ok(final Phi value) {
            super(
                new Attrs(
                    new Attr(
                        "x",
                        new AtComposite(null, self -> value)
                    )
                )
            );
        }
    }

    /**
     * Broken object.
     * @since 0.57
     */
    private static final class Broken extends PhDefault {

        /**
         * Ctor.
         * @param error Error enclosure
         */
        Broken(final Phi error) {
            super(
                new Attrs(
                    new Attr(
                        "x",
                        new AtComposite(
                            null,
                            self -> {
                                throw new EOerror.ExError(error);
                            }
                        )
                    )
                )
            );
        }
    }
}
