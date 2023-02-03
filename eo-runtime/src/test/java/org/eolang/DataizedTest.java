/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
    void logsCorrectly() {
        final Logger log = Logger.getLogger(Dataized.class.getName());
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Handler() {
            @Override
            public void publish(final LogRecord record) {
                logs.add(record);
            }

            @Override
            public void flush() {
                throw new UnsupportedOperationException("#flush()");
            }

            @Override
            public void close() throws SecurityException {
                throw new UnsupportedOperationException("#close()");
            }
        };
        log.addHandler(hnd);
        new Dataized(new Data.ToPhi(1L)).take();
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("intν"),
                Matchers.not(Matchers.containsString("\n"))
            )
        );
    }

    @Test
    void logsWhenException() {
        final Logger log = Logger.getLogger(Dataized.class.getName());
        final Level before = log.getLevel();
        log.setLevel(Level.ALL);
        final List<LogRecord> logs = new LinkedList<>();
        final Handler hnd = new Handler() {
            @Override
            public void publish(final LogRecord record) {
                logs.add(record);
            }

            @Override
            public void flush() {
                throw new UnsupportedOperationException("#flush()");
            }

            @Override
            public void close() throws SecurityException {
                throw new UnsupportedOperationException("#close()");
            }
        };
        log.addHandler(hnd);
        final Phi wrong = new PhIncorrect(Phi.Φ);
        IntStream.range(0, 5).forEach(
            i -> Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Dataized(wrong).take()
            )
        );
        new Dataized(new Data.ToPhi(1L)).take();
        log.setLevel(before);
        log.removeHandler(hnd);
        MatcherAssert.assertThat(
            logs.get(0).getMessage(),
            Matchers.allOf(
                Matchers.containsString("intν"),
                Matchers.not(Matchers.containsString("\n"))
            )
        );
    }

    /**
     * Fake Phi failing when dataized.
     * @since 1.0
     */
    public static class PhIncorrect extends PhDefault {

        /**
         * Ctor.
         * @param sigma Sigma
         */
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

}
