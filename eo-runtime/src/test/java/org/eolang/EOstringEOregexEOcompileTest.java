/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link EOstring$EOregex$EOcompile}.
 * @since 0.57.4
 */
final class EOstringEOregexEOcompileTest {

    @ParameterizedTest
    @CsvSource({
        "/a|ab/, ab, true",
        "/a+?/, aaa, true",
        "/a|ab/i, AB, true",
        "/([0-9]) #ignore this comment/x, 4, true",
        "/(?x)([0-9]) #ignore this comment/, 4, true",
        "/a(?x) #tail/, a, true",
        "/(?x)a(?-x)/, a, true",
        "/\\Qa|ab/, a|ab, true",
        "/a/b|a/bc/, a/bc, true",
        "/[a-z]+/, 1abc, false"
    })
    void matchesEntireTextAfterBacktracking(
        final String expression, final String text, final boolean expected
    ) {
        MatcherAssert.assertThat(
            String.format("regex %s must match the entire text %s", expression, text),
            new Dataized(
                new PhApplication(
                    new Data.ToPhi(expression)
                        .take("regex").take("compiled").take("matches").copy(),
                    "txt", new Data.ToPhi(text)
                )
            ).asBool(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void compilesRegexWithSlashes() {
        MatcherAssert.assertThat(
            "regex \"/[a-z]+/\" should compile and match \"hello\"",
            new Dataized(
                new PhApplication(
                    new Data.ToPhi("/[a-z]+/")
                        .take("regex").take("compiled").take("matches").copy(),
                    "txt", new Data.ToPhi("hello")
                )
            ).asBool(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void throwsClearErrorOnMissingClosingSlash() {
        MatcherAssert.assertThat(
            "regex without closing slash must terminate with a clear reason about the missing slash, not an opaque IndexOutOfBoundsException",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new Data.ToPhi("/pattern").take("regex").take("compiled")
                ).take()
            ).toString(),
            Matchers.allOf(
                Matchers.containsString("slash"),
                Matchers.not(Matchers.containsString("out of bounds"))
            )
        );
    }

    @Test
    void refusesFlagsThatCarryRegexSyntax() {
        MatcherAssert.assertThat(
            "a flag section that closes its own group must be refused, not spliced into the pattern where it silently changes what the regex means",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new Data.ToPhi("/b/i)|(?:a").take("regex").take("compiled")
                ).take()
            ).toString(),
            Matchers.containsString("regex flags 'i)|(?:a' must be a sequence")
        );
    }

    @Test
    void refusesAFlagLetterTheEoSideDoesNotRead() {
        MatcherAssert.assertThat(
            "an unknown flag letter must be reported as a flag, since the EO half of this object reads only [dimsux], rather than as whatever the engine makes of it",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new Data.ToPhi("/b/z").take("regex").take("compiled")
                ).take()
            ).toString(),
            Matchers.allOf(
                Matchers.containsString("regex flags 'z' must be a sequence"),
                Matchers.not(Matchers.containsString("Unknown inline modifier"))
            )
        );
    }
}
