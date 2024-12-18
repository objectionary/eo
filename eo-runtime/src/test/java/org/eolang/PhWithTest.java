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

import com.yegor256.Together;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link PhWith}.
 *
 * @since 0.16
 */
final class PhWithTest {

    @Test
    void comparesTwoObjects() {
        final Phi dummy = new PhWith(
            new PhMethod(new PhWithTest.Dummy(), "plus"),
            0, new Data.ToPhi(1L)
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            dummy, Matchers.equalTo(dummy)
        );
    }

    @Test
    void takesMethod() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new Data.ToPhi("Hello, world!")
            ).asString(),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    void passesToSubObject() {
        final Phi dummy = new PhWithTest.Dummy();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new PhWith(
                    new PhCopy(new PhMethod(dummy, "plus")),
                    0, new Data.ToPhi(1L)
                )
            ).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @Test
    void printsToString() {
        final Phi dummy = new PhWithTest.Dummy();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new PhWith(
                new PhCopy(new PhMethod(dummy, "plus")),
                0, new Data.ToPhi(1L)
            ).toString(),
            Matchers.matchesPattern(".*Dummy.*\\.plus.*\\[#0=EOorg\\.EOeolang\\.EOnumber.*")
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {"hello", "bye", "", "привет"})
    void runsInThreads(final String data) {
        final String attr = "foo";
        final Phi ref = new PhWith(new DummyWithAtFree(attr), 0, new Data.ToPhi(data));
        MatcherAssert.assertThat(
            "works in multiple threads",
            new Together<>(
                thread -> {
                    MatcherAssert.assertThat(
                        AtCompositeTest.TO_ADD_MESSAGE,
                        new Dataized(ref.take(attr)).asString(),
                        Matchers.is(data)
                    );
                    return true;
                }
            ),
            Matchers.not(Matchers.hasItem(false))
        );
    }

    @Test
    void hasTheSameFormaWithBoundAttribute() {
        final Phi dummy = new DummyWithAtFree("x");
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            dummy.forma(),
            Matchers.equalTo(
                new PhWith(dummy, "x", new Data.ToPhi(5L)).forma()
            )
        );
    }

    /**
     * Dummy Phi with free attribute.
     * @since 0.1.0
     */
    private static class DummyWithAtFree extends PhDefault {

        /**
         * Ctor.
         * @param attr Free attribute name
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        DummyWithAtFree(final String attr) {
            this.add(attr, new AtVoid(attr));
        }
    }

    /**
     * Dummy Phi.
     * @since 0.1.0
     */
    public static class Dummy extends PhDefault {

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Dummy() {
            this.add("φ", new AtComposite(this, self -> new Data.ToPhi(1L)));
        }
    }
}
