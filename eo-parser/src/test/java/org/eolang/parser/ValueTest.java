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
@SuppressWarnings("PMD.TooManyMethods")
final class ValueTest {

    @Test
    void retainsKindFromCtor() {
        MatcherAssert.assertThat(
            "kind() must round-trip the ctor tag so the emitter dispatches correctly",
            new Value(Value.Kind.INTEGER, "42", 0, 2).kind(),
            Matchers.equalTo(Value.Kind.INTEGER)
        );
    }

    @Test
    void retainsRawTextFromCtor() {
        MatcherAssert.assertThat(
            "raw() must round-trip the source text untouched",
            new Value(Value.Kind.IDENTIFIER, "foo-bar", 4, 11).raw(),
            Matchers.equalTo("foo-bar")
        );
    }

    @Test
    void retainsPositionFromCtor() {
        MatcherAssert.assertThat(
            "pos() must round-trip the source column for emitter @pos",
            new Value(Value.Kind.STAR, "*", 7, 8).pos(),
            Matchers.equalTo(7)
        );
    }

    @Test
    void retainsEndIndexFromCtor() {
        MatcherAssert.assertThat(
            "end() must round-trip the cursor advance position",
            new Value(Value.Kind.IDENTIFIER, "foo", 0, 3).end(),
            Matchers.equalTo(3)
        );
    }

    @Test
    void exposesEveryKind() {
        MatcherAssert.assertThat(
            "Value.Kind must enumerate every kind the parser currently recognises",
            Value.Kind.values().length,
            Matchers.equalTo(9)
        );
    }

    @Test
    void retainsBytesKind() {
        MatcherAssert.assertThat(
            "BYTES must be one of the recognised value kinds",
            new Value(Value.Kind.BYTES, "CA-FE", 0, 5).kind(),
            Matchers.equalTo(Value.Kind.BYTES)
        );
    }

    @Test
    void retainsHexKind() {
        MatcherAssert.assertThat(
            "HEX must be one of the recognised value kinds for `0xFF` literals",
            new Value(Value.Kind.HEX, "0xFF", 0, 4).kind(),
            Matchers.equalTo(Value.Kind.HEX)
        );
    }

    @Test
    void retainsBindingFromCtor() {
        MatcherAssert.assertThat(
            "binding() must round-trip the inline-binding tag from the ctor",
            new Value(Value.Kind.IDENTIFIER, "a", 0, 1, "y").binding(),
            Matchers.equalTo("y")
        );
    }

    @Test
    void returnsNullBindingWhenAbsent() {
        MatcherAssert.assertThat(
            "binding() must return null when no inline binding was supplied",
            new Value(Value.Kind.IDENTIFIER, "a", 0, 1).binding(),
            Matchers.nullValue()
        );
    }

    @Test
    void retainsGroupKind() {
        MatcherAssert.assertThat(
            "GROUP must be one of the recognised value kinds for paren-bracketed expressions",
            new Value(Value.Kind.GROUP, "(foo)", 0, 5).kind(),
            Matchers.equalTo(Value.Kind.GROUP)
        );
    }

    @Test
    void retainsFloatKind() {
        MatcherAssert.assertThat(
            "FLOAT must be one of the recognised value kinds",
            new Value(Value.Kind.FLOAT, "3.14", 0, 4).kind(),
            Matchers.equalTo(Value.Kind.FLOAT)
        );
    }

    @Test
    void retainsStringKind() {
        MatcherAssert.assertThat(
            "STRING must be one of the recognised value kinds",
            new Value(Value.Kind.STRING, "\"hi\"", 0, 4).kind(),
            Matchers.equalTo(Value.Kind.STRING)
        );
    }

    @Test
    void retainsRootKind() {
        MatcherAssert.assertThat(
            "ROOT must be one of the recognised value kinds for Q/@/^/$",
            new Value(Value.Kind.ROOT, "Q", 0, 1).kind(),
            Matchers.equalTo(Value.Kind.ROOT)
        );
    }
}
