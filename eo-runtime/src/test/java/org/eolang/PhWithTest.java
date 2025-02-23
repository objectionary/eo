/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
