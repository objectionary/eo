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

import org.cactoos.Func;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.llorllale.cactoos.matchers.RunsInThreads;

/**
 * Test case for {@link PhWith}.
 *
 * @since 0.16
 */
final class PhWithTest {

    @Test
    void comparesTwoObjects() {
        final Phi dummy = new PhWith(
            new PhMethod(new PhWithTest.Dummy(Phi.Φ), "plus"),
            0, new Data.ToPhi(1L)
        );
        MatcherAssert.assertThat(
            dummy, Matchers.equalTo(dummy)
        );
    }

    @Test
    void takesMethod() {
        MatcherAssert.assertThat(
            new Dataized(
                new Data.ToPhi("Hello, world!")
            ).take(String.class),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    void passesToSubObject() {
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
    void printsToString() {
        final Phi dummy = new PhWithTest.Dummy(Phi.Φ);
        MatcherAssert.assertThat(
            new PhWith(
                new PhCopy(new PhMethod(dummy, "plus")),
                0, new Data.ToPhi(1L)
            ).copy().toString(),
            Matchers.matchesPattern(".*int\\.plus≡.*EOorg\\.EOeolang\\.EOint\\$EOplusν.*")
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {"hello", "bye", "", "привет"})
    void runsInThreads(final String data) {
        final String attr = "foo";
        final Phi ref = new PhWith(new DummyWithAtFree(attr, Phi.Φ), 0, new Data.ToPhi(data));
        final Func<Phi, Boolean> actual = phi -> {
            MatcherAssert.assertThat(
                new Dataized(phi.attr(attr).get()).take(String.class),
                Matchers.is(data)
            );
            return true;
        };
        MatcherAssert.assertThat(
            actual,
            new RunsInThreads<>(
                ref,
                Runtime.getRuntime().availableProcessors() * 10
            )
        );
    }

    @Test
    void hasTheSameFormaWithBoundAttribute() {
        final Phi dummy = new DummyWithAtFree("x", Phi.Φ);
        MatcherAssert.assertThat(
            dummy.forma(),
            Matchers.equalTo(
                new PhWith(dummy, "x", new Data.Value<>(5L)).forma()
            )
        );
    }

    /**
     * Dummy Phi with free attribute.
     * @since 1.0
     */
    private static class DummyWithAtFree extends PhDefault {

        /**
         * Ctor.
         * @param attr Free attribute name
         * @param sigma Sigma
         */
        DummyWithAtFree(final String attr, final Phi sigma) {
            super(sigma);
            this.add(attr, new AtFree());
        }
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
        Dummy(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(this, self -> new Data.ToPhi(1L)));
        }
    }
}
