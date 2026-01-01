/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import EOorg.EOeolang.EOerror;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
        final Logger log = Logger.getLogger("logsWithPhSafe");
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
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
                log
            ).take(),
            "it is expected to fail with and exception"
        );
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            "all messages should be logged",
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("1) Error in"),
                Matchers.containsString("at foo.bar:0:0"),
                Matchers.containsString("intentional error")
            )
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
        final Logger log = Logger.getLogger("logsWithPhSafe");
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
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
                log
            ).take(),
            "it is expected to fail with and exception"
        );
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            "Messages should not be logged",
            logs,
            Matchers.empty()
        );
    }

    /**
     * Handler implementation for tests.
     *
     * @since 0.1.0
     */
    private static class Hnd extends Handler {
        /**
         * Logs.
         */
        private final List<LogRecord> logs;

        /**
         * Ctor.
         *
         * @param logs Logs
         */
        Hnd(final List<LogRecord> logs) {
            this.logs = logs;
        }

        @Override
        public void publish(final LogRecord record) {
            this.logs.add(record);
        }

        @Override
        public void flush() {
            throw new UnsupportedOperationException("#flush()");
        }

        @Override
        public void close() {
            throw new UnsupportedOperationException("#close()");
        }
    }
}
