/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@link Dataized}.
 *
 * @since 0.22
 */
@Execution(ExecutionMode.SAME_THREAD)
public final class DataizedTest {

    @Test
    public void logsCorrectly() {
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
                Matchers.containsString("int⟦"),
                Matchers.not(Matchers.containsString("\n"))
            )
        );
    }

    /**
     * Try to datarize an EO app that calls a varargs func.
     * @todo #414:30min Fix bug execute an EO program calling a varargs func.
     *  We reproduced by a test bug during execution with exception `You can't overwrite X`
     *  when EO app try to call a function that uses varargs as parameter. Now, we must fix it
     *  and disable the test (test below).
     */
    @Test
    @Disabled
    public void datarizesEOappCallingVarargsFunc() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOappCallingVarargsFunc(Phi.Φ)
            ).take(Long.class),
            Matchers.is(2L)
        );
    }

    /**
     * Main program sample.
     * {@code
     *  [] > app
     *    (f 1 2 3).eq 2 > @
     * }
     * @since 0.22
     */
    private static class EOappCallingVarargsFunc extends PhDefault {
        public EOappCallingVarargsFunc(Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        (rho) -> {
                            Phi fvar = new PhCopy(new DataizedTest.Fvarargs(rho));
                            Phi var1 = new Data.ToPhi(1L);
                            Phi var2 = new Data.ToPhi(2L);
                            Phi var3 = new Data.ToPhi(3L);
                            Phi fvard = new PhWith(fvar, 0, var1);
                            fvard = new PhWith(fvard, 1, var2);
                            fvard = new PhWith(fvard, 2, var3);
                            return new PhWith(
                                new PhCopy(new PhMethod(fvard, "eq")), 0, new Data.ToPhi(2L)
                            );
                        }
                    )
                )
            );
        }
    }

    /**
     * Varargs function sample.
     * {@code
     *  [args] > f
     *    1 > a
     *    2 > @
     * }
     * @since 0.22
     */
    @SuppressWarnings("unchecked")
    private static class Fvarargs extends PhDefault {
        public Fvarargs(final Phi sigma) {
            super(sigma);
            this.add("args", new AtVararg());
            this.add(
                "a",
                new AtOnce(
                    new AtComposite(this, (rho) -> new Data.ToPhi(1L))
                )
            );
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(this, (rho) -> new Data.ToPhi(2L))
                )
            );
        }
    }
}
