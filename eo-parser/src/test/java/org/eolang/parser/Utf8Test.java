/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.charset.StandardCharsets;
import org.cactoos.io.InputOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Utf8}.
 *
 * @since 0.75.0
 */
final class Utf8Test {

    @Test
    void readsMultiByteText() throws Exception {
        MatcherAssert.assertThat(
            "valid multi-byte UTF-8 must come through unchanged, but it didnt",
            new Utf8(
                new InputOf("привет".getBytes(StandardCharsets.UTF_8))
            ).asString(),
            Matchers.equalTo("привет")
        );
    }

    @Test
    void reportsLineOfBadByte() {
        final byte[] bytes = {
            '[', ']', ' ', '>', ' ', 'a', '\n', ' ', ' ', '"',
            (byte) 0xE9, '"', ' ', '>', ' ', 'x', '\n',
        };
        MatcherAssert.assertThat(
            "the error must name the source and the line of the bad byte, but it didnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Utf8(new InputOf(bytes), "a.eo").asString()
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("a.eo:2:"),
                Matchers.containsString("not valid UTF-8")
            )
        );
    }

    @Test
    void reportsTruncatedCharacter() {
        MatcherAssert.assertThat(
            "a truncated character must be reported on its line, but it wasnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Utf8(new InputOf(new byte[] {(byte) 0xD0})).asString()
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("line 1:"),
                Matchers.containsString("not valid UTF-8")
            )
        );
    }

    @Test
    void keepsByteOrderMark() throws Exception {
        final String text = Character.toString(0xFEFF).concat("[] > a");
        MatcherAssert.assertThat(
            "a byte order mark is valid UTF-8 and must come through, but it didnt",
            new Utf8(
                new InputOf(text.getBytes(StandardCharsets.UTF_8))
            ).asString(),
            Matchers.equalTo(text)
        );
    }
}
