/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
            new PhMethod(PhWithTest.Dummy.make(), "plus"),
            0, new Data.ToPhi(1L)
        );
        MatcherAssert.assertThat(
            "PhWith should be equal to itself, but it didn't",
            dummy, Matchers.equalTo(dummy)
        );
    }

    @Test
    void takesMethod() {
        MatcherAssert.assertThat(
            "PhWith should preserve inner method result from Dataized string, but it didn't",
            new Dataized(
                new Data.ToPhi("Hello, world!")
            ).asString(),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    void passesToSubObject() {
        MatcherAssert.assertThat(
            "PhWith should pass attribute to sub-object and calculate correctly, but it didn't",
            new Dataized(
                new PhWith(
                    new PhCopy(new PhMethod(PhWithTest.Dummy.make(), "plus")),
                    0, new Data.ToPhi(1L)
                )
            ).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {"hello", "bye", "", "привет"})
    void runsInThreads(final String data) {
        final Phi ref = new PhWith(DummyWithAtFree.make("foo"), 0, new Data.ToPhi(data));
        MatcherAssert.assertThat(
            "works in multiple threads",
            new Together<>(
                thread -> {
                    MatcherAssert.assertThat(
                        "Attribute 'foo' should return the same string that was passed to Phi, but it didn't",
                        new Dataized(ref.take("foo")).asString(),
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
        final Phi dummy = DummyWithAtFree.make("x");
        MatcherAssert.assertThat(
            "forma of PhWith with bound attribute should be same as forma of original, but it didn't",
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
    private static final class DummyWithAtFree extends PhDefault {

        /**
         * Factory method.
         * @param attr Free attribute name
         * @return New DummyWithAtFree instance
         */
        static DummyWithAtFree make(final String attr) {
            final DummyWithAtFree dummy = new DummyWithAtFree();
            dummy.add(attr, new AtVoid(attr));
            return dummy;
        }
    }

    /**
     * Dummy Phi.
     * @since 0.1.0
     */
    public static final class Dummy extends PhDefault {

        /**
         * Factory method.
         * @return New Dummy instance
         */
        static Dummy make() {
            final Dummy dummy = new Dummy();
            dummy.add("φ", new AtComposite(dummy, self -> new Data.ToPhi(1L)));
            return dummy;
        }
    }
}
