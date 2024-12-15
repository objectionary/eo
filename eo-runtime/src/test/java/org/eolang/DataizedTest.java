/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang;

import EOorg.EOeolang.EOerror;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.stream.IntStream;
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
    /**
     * System property for maximum dataization log level.
     */
    private static final String MAX_DATAIZATION = "max.dataization.log";

    @Test
    void logsCorrectly() {
        final Logger log = Logger.getLogger("logsCorrectly");
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
        new Dataized(new Data.ToPhi(1L), log).take();
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            "Expected correct logs for object dataization",
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("numberν"),
                Matchers.not(Matchers.containsString("\n"))
            )
        );
    }

    @Test
    void logsWhenException() {
        final Logger log = Logger.getLogger("logsWhenException");
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
        IntStream.range(0, 5).forEach(
            i -> Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(new PhIncorrect()).take(),
                "Expected failure with ExFailure exception on incorrect object dataization"
            )
        );
        new Dataized(new Data.ToPhi(1L), log).take();
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            "Expected correct logs for object dataization",
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("numberν"),
                Matchers.not(Matchers.containsString("\n"))
            )
        );
    }

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
                    new PhLocated(new PhIncorrect(), "foo.bar", 0, 0)
                ),
                log
            ).take(),
            "it is expected to fail with ExFailure exception"
        );
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            "all messages should be logged",
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("1) Error in"),
                Matchers.containsString("2) There's no"),
                Matchers.containsString("3)"),
                Matchers.containsString("at foo.bar:0:0"),
                Matchers.containsString("no \"Δ\" in the object of")
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
    void printsShortLogs() throws InterruptedException {
        final Logger log = Logger.getLogger("printsShortLogs");
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
        final Thread thread = new DataizedLoggerThread(log, 1);
        thread.start();
        thread.join();
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            "Number of log records should be equal to 1 in the case of short logs",
            logs.size(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void printsLongLogs() throws InterruptedException {
        final Logger log = Logger.getLogger(Dataized.class.getName());
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
        final Thread thread = new DataizedLoggerThread(log, 2);
        thread.start();
        thread.join();
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            "Number of log records should be greater than 1 the in case of long logs",
            logs.size(),
            Matchers.greaterThan(1)
        );
    }

    /**
     * Thread that logs dataization process with specified log level.
     * @since 0.1.0
     */
    private static class DataizedLoggerThread extends Thread {
        /**
         * Ctor.
         *
         * @param logger Logger.
         * @param level Log level.
         */
        DataizedLoggerThread(final Logger logger, final int level) {
            super(
                () -> {
                    final String property = System.getProperty(
                        DataizedTest.MAX_DATAIZATION
                    );
                    System.getProperties().setProperty(
                        DataizedTest.MAX_DATAIZATION,
                        String.valueOf(level)
                    );
                    final Phi phi = new PhiDec();
                    new Dataized(phi, logger).take();
                    if (property != null) {
                        System.getProperties().setProperty(
                            DataizedTest.MAX_DATAIZATION,
                            property
                        );
                    } else {
                        System.clearProperty(DataizedTest.MAX_DATAIZATION);
                    }
                }
            );
        }
    }

    /**
     * Fake Phi failing when dataized.
     * @since 0.1.0
     */
    private static class PhIncorrect extends PhDefault {

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        PhIncorrect() {
            this.add(
                "Δ",
                new AtComposite(
                    this,
                    rho -> Phi.Φ
                )
            );
        }
    }

    /**
     * Fake Phi with decoration.
     *
     * @since 0.1.0
     */
    public static class PhiDec extends PhDefault {

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        PhiDec() {
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> new PhWith(
                            new PhCopy(new PhMethod(new Data.ToPhi(2L), "plus")),
                            0,
                            new Data.ToPhi(2L)
                        )
                    )
                )
            );
        }
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
