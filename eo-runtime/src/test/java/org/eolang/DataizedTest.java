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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link Dataized}.
 *
 * @since 0.22
 * @todo #2931:30min Enable the disabled tests. The tests were disabled after \rho attribute
 *  became immutable. Need to find out what's going on and resolve the tests.
 */
final class DataizedTest {
    /**
     * System property for maximum dataization log level.
     */
    private static final String DATAIZATION_LOG = "max.dataization.log";

    @Test
    @Disabled
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
            AtCompositeTest.TO_ADD_MESSAGE,
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("intν"),
                Matchers.not(Matchers.containsString("\n"))
            )
        );
    }

    @Test
    @Disabled
    void logsWhenException() {
        final Logger log = Logger.getLogger("logsWhenException");
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
        final Phi wrong = new PhIncorrect(Phi.Φ);
        IntStream.range(0, 5).forEach(
            i -> Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Dataized(wrong).take(),
                AtCompositeTest.TO_ADD_MESSAGE
            )
        );
        new Dataized(new Data.ToPhi(1L), log).take();
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("intν"),
                Matchers.not(Matchers.containsString("\n"))
            )
        );
    }

    @ParameterizedTest
    @ValueSource(ints = {1, 2})
    void printsShortLogs(final int level) throws InterruptedException {
        final Logger log = Logger.getLogger(Dataized.class.getName());
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Hnd(logs);
        log.addHandler(hnd);
        final Thread thread = new Thread(
            () -> {
                final String property = System.getProperty(DataizedTest.DATAIZATION_LOG);
                System.getProperties().setProperty(
                    DataizedTest.DATAIZATION_LOG,
                    String.valueOf(level)
                );
                final Phi phi = new PhiDec(Phi.Φ);
                new Dataized(phi, log).take();
                if (property != null) {
                    System.getProperties().setProperty(DataizedTest.DATAIZATION_LOG, property);
                } else {
                    System.clearProperty(DataizedTest.DATAIZATION_LOG);
                }
            });
        thread.start();
        thread.join();
        log.setLevel(before);
        log.removeHandler(hnd);
        if (level == 1) {
            MatcherAssert.assertThat(
                "Number of log records should be 1 in case of short logs",
                logs.size(),
                Matchers.equalTo(1)
            );
        } else if (level == 2) {
            MatcherAssert.assertThat(
                "Number of log records should be greater than 1 in case of long logs",
                logs.size(),
                Matchers.greaterThan(1)
            );
        }
    }

    /**
     * Fake Phi failing when dataized.
     * @since 1.0
     */
    private static class PhIncorrect extends PhDefault {

        /**
         * Ctor.
         * @param sigma Sigma
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        PhIncorrect(final Phi sigma) {
            super(sigma);
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
     * @since 1.0
     */
    public static class PhiDec extends PhDefault {

        /**
         * Ctor.
         *
         * @param sigma Sigma
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        PhiDec(final Phi sigma) {
            super(sigma);
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
     * @since 1.0
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
