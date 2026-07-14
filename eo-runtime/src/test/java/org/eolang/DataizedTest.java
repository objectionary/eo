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
 * Test case for {@link Dataized}.
 * @since 0.22
 */
final class DataizedTest {

    @Test
    void failsWithLocationThroughPhSafe() {
        MatcherAssert.assertThat(
            "failure keeps the location of the offending object and its cause",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(
                    new PhSafe(
                        new PhDispatch(
                            new PhDefault() {
                                @Override
                                public Phi take(final String name) {
                                    throw new IllegalStateException("intentional error");
                                }
                            },
                            "xyz"
                        ),
                        "foo.bar", 0, 0
                    )
                ).take(),
                "dataization was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("at foo.bar:0:0"),
                Matchers.containsString("intentional error")
            )
        );
    }

    @Test
    void failsWhenForcingTerminated() {
        MatcherAssert.assertThat(
            "forcing a ⊥ fails with the cause it carries",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(PhTerminator.withCause("boom")).take(),
                "forcing a ⊥ was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("boom")
        );
    }

    @Test
    void failsWhenTypeIsUnknown() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(new Data.ToPhi(0L)).take(Object.class),
            "requesting an unsupported type was expected to fail with ExFailure"
        );
    }

    @Test
    void reportsActualLengthWhenBoolIsEmpty() {
        MatcherAssert.assertThat(
            "the message must report the true (zero) length, not claim it's over one",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(new PhDefault(new byte[0])).asBool(),
                "dataizing empty bytes as boolean was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("length 0")
        );
    }
}
