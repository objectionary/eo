/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtVararg}.
 *
 * @since 0.1
 */
public final class AtVarargTest {

    @Test
    public void appendsElements() {
        final Attr attr = new AtVararg();
        final Phi phi = new PhDefault() {
        };
        attr.put(phi);
        attr.put(phi);
        MatcherAssert.assertThat(
            new Dataized(attr.get()).take(Phi[].class)[1],
            Matchers.equalTo(phi)
        );
    }

    @Test
    public void injectsVarargs() {
        final Phi bar = new AtVarargTest.Bar(Phi.Φ);
        bar.attr(0).put(new Data.ToPhi(1L));
        bar.attr(1).put(new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            new Dataized(
                bar.attr("a").get()
            ).take(Long.class),
            Matchers.is(1L)
        );
    }

    @Test
    public void datarizesEOappCallingVarargsFunc() {
        MatcherAssert.assertThat(
            new Dataized(
                new AtVarargTest.Foo(Phi.Φ)
            ).take(Boolean.class),
            Matchers.is(true)
        );
    }

    /**
     * Main program sample.
     *
     * {@code
     *  [] > foo
     *    (bar 1 2 3).eq 2 > @
     * }
     *
     * @since 0.22
     */
    private static class Foo extends PhDefault {
        Foo(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> {
                            Phi phi = new PhWith(
                                new PhCopy(new AtVarargTest.Bar(rho)),
                                0, new Data.ToPhi(1L)
                            );
                            phi = new PhWith(phi, 1, new Data.ToPhi(2L));
                            phi = new PhWith(phi, 2, new Data.ToPhi(3L));
                            return new PhWith(
                                new PhCopy(new PhMethod(phi, "eq")),
                                0, new Data.ToPhi(2L)
                            );
                        }
                    )
                )
            );
        }
    }

    /**
     * Varargs function sample.
     *
     * {@code
     *  [args...] > bar
     *    1 > a
     *    2 > @
     * }
     *
     * @since 0.22
     */
    private static class Bar extends PhDefault {
        Bar(final Phi sigma) {
            super(sigma);
            this.add("args", new AtVararg());
            this.add(
                "a",
                new AtOnce(
                    new AtComposite(this, rho -> new Data.ToPhi(1L))
                )
            );
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(this, rho -> new Data.ToPhi(2L))
                )
            );
        }
    }

}
