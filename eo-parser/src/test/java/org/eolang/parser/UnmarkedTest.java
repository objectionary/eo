/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Unmarked}.
 *
 * @since 0.0.0
 */
final class UnmarkedTest {

    @Test
    void takesTheMarkOffTheFirstLine() throws Exception {
        MatcherAssert.assertThat(
            "the byte order mark must be gone from the front of the text, but it isnt",
            new Unmarked(
                new TextOf(Character.toString(0xFEFF).concat("# The app."))
            ).asString(),
            Matchers.equalTo("# The app.")
        );
    }

    @Test
    void keepsAMarkThatIsNotInFront() throws Exception {
        MatcherAssert.assertThat(
            "a mark the program itself holds must stay where it is, but it didnt",
            new Unmarked(
                new TextOf("\"".concat(Character.toString(0xFEFF)).concat("\" > bom"))
            ).asString(),
            Matchers.equalTo("\"".concat(Character.toString(0xFEFF)).concat("\" > bom"))
        );
    }

    @Test
    void readsAnEmptyTextAsItIs() throws Exception {
        MatcherAssert.assertThat(
            "an empty source must come through untouched, but it didnt",
            new Unmarked(new TextOf("")).asString(),
            Matchers.equalTo("")
        );
    }
}
