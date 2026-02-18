/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import EOorg.EOeolang.EOerror;
import java.util.logging.Logger;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@link Dataized}.
 *
 * @since 0.22
 */
@Execution(ExecutionMode.SAME_THREAD)
final class DataizedTest {
    @Test
    void logsAllLocationsWithPhSafe() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(
                new PhSafe(
                    new PhMethod(
                        new PhDefault() {
                            @Override
                            public Phi take(final String name) {
                                throw new IllegalStateException("intentional error");
                            }
                        },
                        "xyz"
                    ),
                    "foo.bar", 0, 0
                ),
                Logger.getLogger("logsWithPhSafe")
            ).take(),
            "Dataizing PhSafe with error should throw ExError, but it didn't"
        );
    }

    @Test
    void failsWhenError() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(
                new PhWith(
                    new EOerror(),
                    "message",
                    new Data.ToPhi("hello")
                )
            ).take(),
            "re-throws when dataization fails with 'error' object"
        );
    }

    @Test
    void doesNotLogGoToTokenJump() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(
                new PhDefault() {
                    @Override
                    public byte[] delta() {
                        throw new EOerror.ExError(
                            Phi.Φ.take("org")
                                .take("eolang")
                                .take("go")
                                .take("to")
                                .take("token")
                                .take("jump")
                        );
                    }
                },
                Logger.getLogger("logsWithPhSafe")
            ).take(),
            "Dataizing object with ExError should rethrow, but it didn't"
        );
    }
}
