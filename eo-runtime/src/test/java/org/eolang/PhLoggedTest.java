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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhLogged}.
 *
 * @since 0.29.0
 */
class PhLoggedTest {

    @Test
    void getsOriginAttributeByPos() {
        final Dummy phi = new Dummy(Phi.Φ);
        MatcherAssert.assertThat(
            new PhLogged(phi).attr(0).toString(),
            Matchers.equalTo(phi.attr("x").toString())
        );
    }

    @Test
    void getsOriginAttributeByName() {
        MatcherAssert.assertThat(
            new PhLogged(Phi.Φ).attr("org"),
            Matchers.instanceOf(AtLogged.class)
        );
    }

    @Test
    void convertsToOriginTerm() {
        MatcherAssert.assertThat(
            new PhLogged(Phi.Φ).φTerm(),
            Matchers.is(Phi.Φ.φTerm())
        );
    }

    @Test
    void copiesOrigin() {
        MatcherAssert.assertThat(
            new PhLogged(Phi.Φ).copy(),
            Matchers.equalTo(Phi.Φ)
        );
    }

    @Test
    void returnsOriginHashCode() {
        MatcherAssert.assertThat(
            new PhLogged(Phi.Φ).hashCode(),
            Matchers.equalTo(Phi.Φ.hashCode())
        );
    }

    @Test
    void equalsToOrigin() {
        MatcherAssert.assertThat(
            new PhLogged(Phi.Φ),
            Matchers.equalTo(Phi.Φ)
        );
    }

    @Test
    void getsOriginLocator() {
        final Phi phi = Phi.Φ;
        MatcherAssert.assertThat(
            new PhLogged(phi).locator(),
            Matchers.equalTo(phi.locator())
        );
    }

    @Test
    void convertsToString() {
        final Phi phi = Phi.Φ;
        MatcherAssert.assertThat(
            new PhLogged(phi).toString(),
            Matchers.equalTo(phi.toString())
        );
    }

    /**
     * Dummy Phi.
     * @since 0.29.0
     */
    private static final class Dummy extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        private Dummy(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
        }
    }
}
