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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtConst}.
 *
 * @since 0.29.0
 */
class AtConstTest {

    @Test
    void convertsToString() {
        MatcherAssert.assertThat(
            new AtConst(Phi.Φ, Phi.Φ, "org").toString(),
            Matchers.equalTo("Φ.orgF!")
        );
    }

    @Test
    void doesNotWrapData() {
        final Phi ret = new AtConst(new Obj(), Phi.Φ, "data").get();
        MatcherAssert.assertThat(
            "Data attribute should not be wrapped with PhFakeRho",
            ret,
            Matchers.not(
                Matchers.instanceOf(PhFakeRho.class)
            )
        );
        MatcherAssert.assertThat(
            "Data attribute should not be wrapped at all",
            ret, Matchers.instanceOf(Data.class)
        );
    }

    @Test
    void wrapsRegularPhiWithPhFakeRho() {
        final Phi ret = new AtConst(new Obj(), Phi.Φ, "attr").get();
        MatcherAssert.assertThat(
            "Regular object should be wrapped with PhFakeRho decorator",
            ret, Matchers.instanceOf(PhFakeRho.class)
        );
    }

    @Test
    void cachesObject() {
        final Phi obj = new Obj();
        final Attr attr = new AtConst(obj, Phi.Φ, "composite");
        final Phi first = attr.get();
        final Phi second = attr.get();
        MatcherAssert.assertThat(
            "AtConst should cache kept object",
            first, Matchers.equalTo(second)
        );
        MatcherAssert.assertThat(
            "Cached attribute should be calculated only once",
            new Dataized(obj.attr("counter").get()).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void shouldFailOnPut() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new AtConst(new Obj(), Phi.Φ, "attr").put(Phi.Φ),
            "AtConst should allow attributes injection"
        );
    }

    @Test
    void shouldFailOnCopy() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new AtConst(new Obj(), Phi.Φ, "attr").copy(Phi.Φ),
            "AtConst is not supposed to be copied"
        );
    }

    @Test
    void fakesResultRho() {
        final Phi obj = new Obj().copy();
        final Phi rho = new PhFake(() -> new Data.ToPhi(10L));
        MatcherAssert.assertThat(
            "Result \\rho object of obj.attr should be faked",
            new AtConst(obj, rho, "attr").get().attr(Attr.RHO).get(),
            Matchers.equalTo(rho)
        );
    }

    /**
     * Object to be cached.
     * @since 0.36.0
     */
    private static class Obj extends PhDefault {
        /**
         * Count.
         */
        private long count = 0;

        /**
         * Ctor.
         */
        Obj() {
            super(Phi.Φ);
            this.add("attr", new AtSimple(new Attribute(this)));
            this.add("data", new AtSimple(new Data.Value<>("Hello")));
            this.add("composite", new AtComposite(this, rho -> {
                this.count++;
                return new Attribute(rho);
            }));
            this.add("counter", new AtFormed(() -> new Data.ToPhi(this.count)));
        }
    }

    /**
     * Attribute of cached object.
     * @since 0.36.0
     */
    private static class Attribute extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Attribute(final Phi sigma) {
            super(sigma);
        }
    }
}
