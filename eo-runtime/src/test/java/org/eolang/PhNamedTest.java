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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhNamed}.
 *
 * @since 0.18
 */
final class PhNamedTest {

    @Test
    void comparesTwoObjects() {
        MatcherAssert.assertThat(
            "two objects are equal",
            new PhNamed(new Data.ToPhi(1L), ""),
            Matchers.not(Matchers.equalTo(new PhNamed(new Data.ToPhi(1L), "")))
        );
        MatcherAssert.assertThat(
            "two objects are not equal",
            new PhNamed(new Data.ToPhi(1L), ""),
            Matchers.not(Matchers.equalTo(new PhNamed(new Data.ToPhi(42L), "")))
        );
    }

    @Test
    void reportsDeltaName() {
        MatcherAssert.assertThat(
            "reports delta attribute name correctly",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new PhNamed(
                    new PhDefault() {
                        @Override
                        public byte[] delta() {
                            throw new ExFailure("oops");
                        }
                    },
                    "self"
                ).delta()
            ).getMessage(),
            Matchers.containsString("self.Δ")
        );
    }

    @Test
    void reportsLambdaName() throws Exception {
        MatcherAssert.assertThat(
            "reports lambda attribute name correctly",
            PhNamedTest.stacktrace(
                Assertions.assertThrows(
                    ExAbstract.class,
                    () -> new PhNamed(
                        new BadLambda(),
                        "here"
                    ).lambda()
                )
            ),
            Matchers.allOf(
                Matchers.containsString("bad lambda oops"),
                Matchers.containsString("here.λ")
            )
        );
    }

    @Test
    void composesErrorMessages() throws Exception {
        MatcherAssert.assertThat(
            "composes errors and reports attribute name correctly",
            PhNamedTest.stacktrace(
                Assertions.assertThrows(
                    ExAbstract.class,
                    () -> new PhNamed(
                        new PhWith(
                            new BadLambda(),
                            "foo",
                            new PhDefault() {
                                @Override
                                public byte[] delta() {
                                    throw new ExFailure("intentional oops");
                                }
                            }
                        ),
                        "entry"
                    ).take("foo").delta()
                )
            ),
            Matchers.allOf(
                Matchers.containsString("intentional oops"),
                Matchers.containsString("entry.foo.Δ")
            )
        );
    }

    /**
     * Print the entire stacktrace of the exception.
     * @param thr The exception
     * @return Full stack trace
     * @throws UnsupportedEncodingException If something wrong with the encoding
     */
    private static String stacktrace(final Throwable thr)
        throws UnsupportedEncodingException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        thr.printStackTrace(new PrintStream(baos));
        return baos.toString(StandardCharsets.UTF_8.name());
    }

    /**
     * Test class, always fails.
     *
     * @since 0.42.0
     */
    class BadLambda extends PhDefault implements Atom {
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        BadLambda() {
            this.add("foo", new AtVoid("foo"));
        }

        @Override
        public Phi lambda() {
            throw new ExFailure("bad lambda oops");
        }
    }

}
