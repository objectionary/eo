/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Value}.
 * @since 0.1
 */
final class ValueTest {

    @Test
    void retainsKindFromCtor() {
        MatcherAssert.assertThat(
            "kind() must round-trip the ctor tag so the emitter dispatches correctly",
            new Value(Value.Kind.INTEGER, "42", 0).kind(),
            Matchers.equalTo(Value.Kind.INTEGER)
        );
    }

    @Test
    void retainsRawTextFromCtor() {
        MatcherAssert.assertThat(
            "raw() must round-trip the source text untouched",
            new Value(Value.Kind.IDENTIFIER, "foo-bar", 4).raw(),
            Matchers.equalTo("foo-bar")
        );
    }

    @Test
    void retainsPositionFromCtor() {
        MatcherAssert.assertThat(
            "pos() must round-trip the source column for emitter @pos",
            new Value(Value.Kind.STAR, "*", 7).pos(),
            Matchers.equalTo(7)
        );
    }

    @Test
    void exposesEveryKind() {
        MatcherAssert.assertThat(
            "Value.Kind must enumerate every kind the parser currently recognises",
            Value.Kind.values().length,
            Matchers.equalTo(11)
        );
    }

    @Test
    void retainsBytesKind() {
        MatcherAssert.assertThat(
            "BYTES must be one of the recognised value kinds",
            new Value(Value.Kind.BYTES, "CA-FE", 0).kind(),
            Matchers.equalTo(Value.Kind.BYTES)
        );
    }

    @Test
    void retainsHexKind() {
        MatcherAssert.assertThat(
            "HEX must be one of the recognised value kinds for `0xFF` literals",
            new Value(Value.Kind.HEX, "0xFF", 0).kind(),
            Matchers.equalTo(Value.Kind.HEX)
        );
    }

    @Test
    void retainsBindingFromCtor() {
        MatcherAssert.assertThat(
            "binding() must round-trip the inline-binding tag from the ctor",
            new Value(Value.Kind.IDENTIFIER, "a", 0, "y").binding(),
            Matchers.equalTo("y")
        );
    }

    @Test
    void returnsEmptyBindingWhenAbsent() {
        MatcherAssert.assertThat(
            "binding() must return an empty string when no inline binding was supplied",
            new Value(Value.Kind.IDENTIFIER, "a", 0).binding(),
            Matchers.equalTo("")
        );
    }

    @Test
    void isBoundWhenLabelGiven() {
        MatcherAssert.assertThat(
            "bound() must be true when the ctor received an inline-binding tag",
            new Value(Value.Kind.IDENTIFIER, "a", 0, "y").bound(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void isNotBoundWhenAbsent() {
        MatcherAssert.assertThat(
            "bound() must be false when no inline binding was supplied",
            new Value(Value.Kind.IDENTIFIER, "a", 0).bound(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void retainsGroupKind() {
        MatcherAssert.assertThat(
            "GROUP must be one of the recognised value kinds for paren-bracketed expressions",
            new Value(Value.Kind.GROUP, "(foo)", 0).kind(),
            Matchers.equalTo(Value.Kind.GROUP)
        );
    }

    @Test
    void retainsFloatKind() {
        MatcherAssert.assertThat(
            "FLOAT must be one of the recognised value kinds",
            new Value(Value.Kind.FLOAT, "3.14", 0).kind(),
            Matchers.equalTo(Value.Kind.FLOAT)
        );
    }

    @Test
    void retainsStringKind() {
        MatcherAssert.assertThat(
            "STRING must be one of the recognised value kinds",
            new Value(Value.Kind.STRING, "\"hi\"", 0).kind(),
            Matchers.equalTo(Value.Kind.STRING)
        );
    }

    @Test
    void retainsRootKind() {
        MatcherAssert.assertThat(
            "ROOT must be one of the recognised value kinds for Q/@/^/$",
            new Value(Value.Kind.ROOT, "Q", 0).kind(),
            Matchers.equalTo(Value.Kind.ROOT)
        );
    }

    @Test
    void retainsTermKind() {
        MatcherAssert.assertThat(
            "TERM must be one of the recognised value kinds for the terminator term T",
            new Value(Value.Kind.TERM, "T", 0).kind(),
            Matchers.equalTo(Value.Kind.TERM)
        );
    }

    @Test
    void retainsIdentityKind() {
        MatcherAssert.assertThat(
            "IDENTITY must be one of the recognised value kinds for the identity object I",
            new Value(Value.Kind.IDENTITY, "I", 0).kind(),
            Matchers.equalTo(Value.Kind.IDENTITY)
        );
    }

    @Test
    void marksIdentifierChainable() {
        MatcherAssert.assertThat(
            "an IDENTIFIER value must allow a .method chain behind it",
            new Value(Value.Kind.IDENTIFIER, "foo", 0).chainable(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void marksHexChainable() {
        MatcherAssert.assertThat(
            "a HEX value must allow a .method chain behind it",
            new Value(Value.Kind.HEX, "0xF00D", 0).chainable(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void marksStarChainable() {
        MatcherAssert.assertThat(
            "a STAR tuple marker must allow a .method chain behind it, as R-3.6 spells out with `*.with 1`",
            new Value(Value.Kind.STAR, "*", 0).chainable(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void marksIdentifierReversible() {
        MatcherAssert.assertThat(
            "an IDENTIFIER value must be allowed as a reversed-dispatch head",
            new Value(Value.Kind.IDENTIFIER, "foo", 0).reversible(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void mapsRootGlyphQToUpperPhi() {
        MatcherAssert.assertThat(
            "rootSymbol() must map Q to the top-level Φ per §9.3",
            new Value(Value.Kind.ROOT, "Q", 0).rootSymbol(),
            Matchers.equalTo("Φ")
        );
    }

    @Test
    void mapsRootGlyphCaretToRho() {
        MatcherAssert.assertThat(
            "rootSymbol() must map ^ to ρ per §9.3",
            new Value(Value.Kind.ROOT, "^", 0).rootSymbol(),
            Matchers.equalTo("ρ")
        );
    }

    @Test
    void marksFloatAsNumber() {
        MatcherAssert.assertThat(
            "number() must be true for a FLOAT value",
            new Value(Value.Kind.FLOAT, "3.14", 0).number(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void marksHexAsHex() {
        MatcherAssert.assertThat(
            "hex() must be true for a HEX value",
            new Value(Value.Kind.HEX, "0xFF", 0).hex(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void marksTermAsTerm() {
        MatcherAssert.assertThat(
            "term() must be true for a TERM value",
            new Value(Value.Kind.TERM, "T", 0).term(),
            Matchers.equalTo(true)
        );
    }
}
