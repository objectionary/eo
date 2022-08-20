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

import EOorg.EOeolang.EOtxt.EOsprintf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhWith}.
 *
 * @since 0.16
 */
public final class PhWithTest {

    @Test
    public void comparesTwoObjects() {
        final Phi dummy = new PhWith(
            new PhMethod(new PhWithTest.Dummy(Phi.Φ), "plus"),
            0, new Data.ToPhi(1L)
        );
        MatcherAssert.assertThat(
            dummy, Matchers.equalTo(dummy)
        );
    }

    @Test
    public void takesMethod() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new EOsprintf(Phi.Φ),
                    0, new Data.ToPhi("Hello, world!")
                )
            ).take(String.class),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    public void passesToSubObject() {
        final Phi dummy = new PhWithTest.Dummy(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhCopy(new PhMethod(dummy, "plus")),
                    0, new Data.ToPhi(1L)
                )
            ).take(Long.class),
            Matchers.equalTo(2L)
        );
    }

    @Test
    public void printsToString() {
        final Phi dummy = new PhWithTest.Dummy(Phi.Φ);
        MatcherAssert.assertThat(
            new PhWith(
                new PhCopy(new PhMethod(dummy, "plus")),
                0, new Data.ToPhi(1L)
            ).copy().toString(),
            Matchers.containsString("int.plus≡EOorg.EOeolang.EOint$EOplusν")
        );
    }

    /**
     * Dummy Phi.
     * @since 1.0
     */
    public static class Dummy extends PhDefault {

        /**
         * Ctor.
         * @param sigma Sigma
         */
        public Dummy(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(this, self -> new Data.ToPhi(1L)));
        }
    }

}
