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
 * Test cases for {@link AtFakeRho}.
 * @since 0.36.0
 */
public class AtFakeRhoTest {
    @Test
    void failsOnCopy() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new AtFakeRho(new AtFree(), Phi.Φ, Phi.Φ).copy(Phi.Φ),
            "AtFakeRho is not supposed to be copied at all"
        );
    }

    @Test
    void failsOnPut() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new AtFakeRho(new AtFree(), Phi.Φ, Phi.Φ).put(Phi.Φ),
            "AtFakeRho is not supposed to be changed at all"
        );
    }

    @Test
    void failsWhenTriesToGetAbsentRhoFromOrigin() {
        final Phi parent = new Parent();
        Assertions.assertThrows(
            ExUnset.class,
            new AtFakeRho(
                parent.attr("child").get().attr(Attr.RHO),
                parent,
                new PhFake(() -> new Data.ToPhi(10L))
            )::get,
            "Should fail when tries to fake absent rho"
        );
    }

    @Test
    void fakesRhoIfGivenParentIsEqualToRealRho() {
        final Phi parent = new Parent().copy();
        MatcherAssert.assertThat(
            "Rho should be faked if given parent is equal to real rho",
            new Dataized(
                new AtFakeRho(
                    parent.attr("child").get().attr(Attr.RHO),
                    parent,
                    new PhFake(() -> new Data.ToPhi(42L))
                ).get()
            ).take(Long.class),
            Matchers.equalTo(42L)
        );
    }

    @Test
    void doesNotFakeRhoIfGivenParentIsNotEqualToRealRho() {
        MatcherAssert.assertThat(
            "Rho should not be faked if given parent is not equal to real rho",
            new Dataized(
                new AtFakeRho(
                    new Data.ToPhi(42L).attr("plus").get().attr(Attr.RHO),
                    new Data.ToPhi("Hello"),
                    new PhFake(() -> new Data.ToPhi(12.2))
                ).get()
            ).take(Long.class),
            Matchers.equalTo(42L)
        );
    }

    /**
     * Parent object.
     * @since 0.36.0
     */
    private static class Parent extends PhDefault {
        /**
         * Ctor.
         */
        Parent() {
            super(Phi.Φ);
            this.add("child", new AtSimple(new Child(this)));
        }
    }

    /**
     * Child object.
     * @since 0.36.0
     */
    private static class Child extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Child(final Phi sigma) {
            super(sigma);
        }
    }
}
