/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link EOregex$EOcompile}.
 * @since 0.57.4
 */
final class EOregexEOcompileTest {

    @ParameterizedTest
    @CsvSource({
        "/a|ab/, ab, true",
        "/a+?/, aaa, true",
        "/[a-z]+/, 1abc, false"
    })
    void matchesEntireTextAfterBacktracking(
        final String expression, final String text, final boolean expected
    ) {
        MatcherAssert.assertThat(
            String.format("regex %s must match the entire text %s", expression, text),
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        Phi.Φ.take("string.regex").copy(),
                        "expression", new Data.ToPhi(expression)
                    ).take("compiled").take("matches").copy(),
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
                    new PhApplication(
                        Phi.Φ.take("string.regex").copy(),
                        "expression", new Data.ToPhi("/[a-z]+/")
                    ).take("compiled").take("matches").copy(),
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
                    new PhApplication(
                        Phi.Φ.take("string.regex").copy(),
                        "expression", new Data.ToPhi("/pattern")
                    ).take("compiled")
                ).take()
            ).toString(),
            Matchers.allOf(
                Matchers.containsString("slash"),
                Matchers.not(Matchers.containsString("out of bounds"))
            )
        );
    }
}
