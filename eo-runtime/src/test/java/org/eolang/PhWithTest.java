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
 * @since 0.16
 */
final class PhWithTest {

    @Test
    void rendersMethodApplicationOnNumberAsTerm() {
        MatcherAssert.assertThat(
            "Method application on a number must render readably in φ-term, but it didnt",
            new PhWith(
                new PhMethod(new Data.ToPhi(5L), "times"), 0, new Data.ToPhi(6L)
            ).φTerm(),
            Matchers.equalTo("5.times(0->6)")
        );
    }

    @Test
    void rendersNumberConstructionAsValue() {
        MatcherAssert.assertThat(
            "Number construction chain must render as its value, but it didnt",
            new PhWith(
                new PhCopy(new PhMethod(Phi.Φ, "number")), 0,
                new PhWith(
                    new PhCopy(new PhMethod(Phi.Φ, "bytes")), 0,
                    new PhDefault(
                        new byte[] {
                            (byte) 0x40, (byte) 0x45, (byte) 0x00, (byte) 0x00,
                            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                        }
                    )
                )
            ).φTerm(),
            Matchers.equalTo("42")
        );
    }

    @Test
    void rendersStringConstructionAsValue() {
        MatcherAssert.assertThat(
            "String construction chain must render as its quoted value, but it didnt",
            new PhWith(
                new PhCopy(new PhMethod(Phi.Φ, "string")), 0,
                new PhWith(
                    new PhCopy(new PhMethod(Phi.Φ, "bytes")), 0,
                    new PhDefault(new byte[] {(byte) 0x68, (byte) 0x69})
                )
            ).φTerm(),
            Matchers.equalTo("\"hi\"")
        );
    }

    @Test
    void rendersPositionalBindingAsTerm() {
        MatcherAssert.assertThat(
            "PhWith must render positional binding in φ-term, but it didnt",
            new PhWith(new PhDefault(), 0, new PhDefault(new byte[] {(byte) 0x2A})).φTerm(),
            Matchers.equalTo("[](0->[D> 2A])")
        );
    }

    @Test
    void rendersNamedBindingAsTerm() {
        MatcherAssert.assertThat(
            "PhWith must render named binding in φ-term, but it didnt",
            new PhWith(new PhDefault(), "x", new PhDefault(new byte[] {(byte) 0x2A})).φTerm(),
            Matchers.equalTo("[](x->[D> 2A])")
        );
    }

    @Test
    void comparesTwoObjects() {
        final Phi dummy = new PhWith(
            new PhMethod(new PhWithTest.Dummy(), "plus"),
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
                    new PhCopy(new PhMethod(new PhWithTest.Dummy(), "plus")),
                    0, new Data.ToPhi(1L)
                )
            ).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {"hello", "bye", "", "привет"})
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void runsInThreads(final String data) {
        final String attr = "foo";
        final Phi ref = new PhWith(new PhWithTest.DummyWithAtFree(attr), 0, new Data.ToPhi(data));
        MatcherAssert.assertThat(
            "works in multiple threads",
            new Together<>(
                thread -> {
                    MatcherAssert.assertThat(
                        "Attribute 'foo' should return the same string that was passed to Phi, but it didn't",
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
        final Phi dummy = new PhWithTest.DummyWithAtFree("x");
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
         * Ctor.
         * @param attr Free attribute name
         */
        DummyWithAtFree(final String attr) {
            super(new Attrs(new Attr(attr, new AtVoid(attr))));
        }
    }

    /**
     * Dummy Phi.
     * @since 0.1.0
     */
    static final class Dummy extends PhDefault {

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (5 lines)
         */
        Dummy() {
            this.add("φ", new AtComposite(this, self -> new Data.ToPhi(1L)));
        }
    }
}
