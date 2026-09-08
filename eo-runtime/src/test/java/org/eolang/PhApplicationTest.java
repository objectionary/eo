/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Together;
import java.nio.charset.StandardCharsets;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link PhApplication}.
 * @since 0.16
 */
final class PhApplicationTest {

    @Test
    void rendersSeveralBindingsAsTerm() {
        MatcherAssert.assertThat(
            "PhApplication with several bindings must render them comma-separated, but it didnt",
            new PhApplication(
                new PhDefault(),
                new Bind(0, new PhDefault(new byte[] {(byte) 0x01})),
                new Bind(1, new PhDefault(new byte[] {(byte) 0x02}))
            ).φTerm(),
            Matchers.equalTo("[](0->[D> 01-],1->[D> 02-])")
        );
    }

    @Test
    void keepsANamedBindingOfANumberStructural() {
        MatcherAssert.assertThat(
            "a named binding is no literal and must render as the application it is, but it read as a number",
            new PhApplication(
                new PhDispatch(Phi.Φ, "number"),
                "wrong",
                new PhApplication(
                    new PhDispatch(Phi.Φ, "bytes"),
                    0,
                    new PhDefault(
                        new byte[] {
                            (byte) 0x40, (byte) 0x45, (byte) 0x00, (byte) 0x00,
                            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                        }
                    )
                )
            ).φTerm(),
            Matchers.startsWith("Φ.number(wrong->")
        );
    }

    @Test
    void appliesSeveralBindingsToObject() {
        MatcherAssert.assertThat(
            "PhApplication must bind every pair to the object, but it didnt",
            new Dataized(
                new PhApplication(
                    new PhDefault(
                        new Attrs(new Attr("a", new AtVoid("a")), new Attr("b", new AtVoid("b")))
                    ),
                    new Bind("a", new Data.ToPhi(1L)),
                    new Bind("b", new Data.ToPhi(2L))
                ).take("b")
            ).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @Test
    void continuesPartialApplicationPositionally() {
        MatcherAssert.assertThat(
            "Positional application must continue into the next unset void, but it didnt",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new PhDefault(
                            new Attrs(
                                new Attr("a", new AtVoid("a")),
                                new Attr("b", new AtVoid("b"))
                            )
                        ),
                        0, new Data.ToPhi(1L)
                    ),
                    0, new Data.ToPhi(2L)
                ).take("b")
            ).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @Test
    void rendersMethodApplicationOnNumberAsTerm() {
        MatcherAssert.assertThat(
            "Method application on a number must render readably in φ-term, but it didnt",
            new PhApplication(
                new PhDispatch(new Data.ToPhi(5L), "times"), 0, new Data.ToPhi(6L)
            ).φTerm(),
            Matchers.equalTo("5.times(0->6)")
        );
    }

    @Test
    void rendersNumberConstructionAsValue() {
        MatcherAssert.assertThat(
            "Number construction chain must render as its value, but it didnt",
            new PhApplication(
                new PhDispatch(Phi.Φ, "number"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "bytes"), 0,
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
            new PhApplication(
                new PhDispatch(Phi.Φ, "string"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "bytes"), 0,
                    new PhDefault(new byte[] {(byte) 0x68, (byte) 0x69})
                )
            ).φTerm(),
            Matchers.equalTo("\"hi\"")
        );
    }

    @Test
    void escapesStringLiteralCharacters() {
        MatcherAssert.assertThat(
            "String φ-term must escape EO string literal characters",
            new PhApplication(
                new PhDispatch(Phi.Φ, "string"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "bytes"), 0,
                    new PhDefault(
                        new byte[] {'a', '"', '\\', '\b', '\f', '\n', '\r', '\t'}
                    )
                )
            ).φTerm(),
            Matchers.equalTo("\"a\\\"\\\\\\b\\f\\n\\r\\t\"")
        );
    }

    @Test
    void escapesControlCharactersOutsideTheSevenHandwritten() {
        MatcherAssert.assertThat(
            "String φ-term must escape control characters like ESC and DEL, but it didn't",
            new PhApplication(
                new PhDispatch(Phi.Φ, "string"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "bytes"), 0,
                    new PhDefault(new byte[] {0x1B, 0x7F})
                )
            ).φTerm(),
            Matchers.equalTo("\"\\u001b\\u007f\"")
        );
    }

    @Test
    void rendersInvalidUtfAsBytes() {
        MatcherAssert.assertThat(
            "Invalid UTF-8 must not be rendered as a misleading string literal",
            new PhApplication(
                new PhDispatch(Phi.Φ, "string"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "bytes"), 0,
                    new PhDefault(new byte[] {(byte) 0xC3, (byte) 0x28})
                )
            ).φTerm(),
            Matchers.equalTo("Φ.string(0->Φ.bytes(0->[D> C3-28]))")
        );
    }

    @Test
    void keepsAStringSpellingADataBlockStructural() {
        MatcherAssert.assertThat(
            "a string whose text spells a data block must render as its text, but it read as bytes",
            new PhApplication(
                new PhDispatch(Phi.Φ, "string"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "string"), 0,
                    new PhApplication(
                        new PhDispatch(Phi.Φ, "bytes"), 0,
                        new PhDefault("[D> 41-42]".getBytes(StandardCharsets.UTF_8))
                    )
                )
            ).φTerm(),
            Matchers.equalTo("Φ.string(0->\"[D> 41-42]\")")
        );
    }

    @Test
    void keepsAStringSpellingAMalformedDataBlockStructural() {
        MatcherAssert.assertThat(
            "a string whose text spells a malformed data block must render, but it died",
            new PhApplication(
                new PhDispatch(Phi.Φ, "string"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "string"), 0,
                    new PhApplication(
                        new PhDispatch(Phi.Φ, "bytes"), 0,
                        new PhDefault("[D> A--B]".getBytes(StandardCharsets.UTF_8))
                    )
                )
            ).φTerm(),
            Matchers.equalTo("Φ.string(0->\"[D> A--B]\")")
        );
    }

    @Test
    void keepsANumberSpellingADataBlockStructural() {
        MatcherAssert.assertThat(
            "a number applied to such a string must render as the application, but it didnt",
            new PhApplication(
                new PhDispatch(Phi.Φ, "number"), 0,
                new PhApplication(
                    new PhDispatch(Phi.Φ, "string"), 0,
                    new PhApplication(
                        new PhDispatch(Phi.Φ, "bytes"), 0,
                        new PhDefault(
                            "[D> 40-45-00-00-00-00-00-00]".getBytes(StandardCharsets.UTF_8)
                        )
                    )
                )
            ).φTerm(),
            Matchers.equalTo("Φ.number(0->\"[D> 40-45-00-00-00-00-00-00]\")")
        );
    }

    @Test
    void rendersPositionalBindingAsTerm() {
        MatcherAssert.assertThat(
            "PhApplication must render positional binding in φ-term, but it didnt",
            new PhApplication(new PhDefault(), 0, new PhDefault(new byte[] {(byte) 0x2A})).φTerm(),
            Matchers.equalTo("[](0->[D> 2A-])")
        );
    }

    @Test
    void rendersNamedBindingAsTerm() {
        MatcherAssert.assertThat(
            "PhApplication must render named binding in φ-term, but it didnt",
            new PhApplication(
                new PhDefault(), "x", new PhDefault(new byte[] {(byte) 0x2A})
            ).φTerm(),
            Matchers.equalTo("[](x->[D> 2A-])")
        );
    }

    @Test
    void comparesTwoObjects() {
        final Phi dummy = new PhApplication(
            new PhDispatch(new PhApplicationTest.Dummy(), "plus"),
            0, new Data.ToPhi(1L)
        );
        MatcherAssert.assertThat(
            "PhApplication should be equal to itself, but it didn't",
            dummy, Matchers.equalTo(dummy)
        );
    }

    @Test
    void takesMethod() {
        MatcherAssert.assertThat(
            "PhApplication should preserve inner method result from Dataized string, but it didn't",
            new Dataized(
                new Data.ToPhi("Hello, world!")
            ).asString(),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    void passesToSubObject() {
        MatcherAssert.assertThat(
            "PhApplication should pass attribute to sub-object correctly, but it didn't",
            new Dataized(
                new PhApplication(
                    new PhDispatch(new PhApplicationTest.Dummy(), "plus"),
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
        final Phi ref = new PhApplication(
            new PhApplicationTest.DummyWithAtFree(attr), 0, new Data.ToPhi(data)
        );
        MatcherAssert.assertThat(
            "works in multiple threads",
            new Together<>(
                thread -> {
                    MatcherAssert.assertThat(
                        "Attribute 'foo' must return the passed string, but it didn't",
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
        final Phi dummy = new PhApplicationTest.DummyWithAtFree("x");
        MatcherAssert.assertThat(
            "forma of PhApplication with bound attribute should match the original, but it didn't",
            dummy.forma(),
            Matchers.equalTo(
                new PhApplication(dummy, "x", new Data.ToPhi(5L)).forma()
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
         */
        Dummy() {
            super(
                new Attrs(
                    new Attr("φ", new AtComposite(new PhDefault(), self -> new Data.ToPhi(1L)))
                )
            );
        }
    }
}
