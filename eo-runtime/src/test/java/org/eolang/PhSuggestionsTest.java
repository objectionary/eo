/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhSuggestions}.
 * @since 1.0
 */
final class PhSuggestionsTest {

    @Test
    void ranksClosestObjectFirst() {
        MatcherAssert.assertThat(
            "Closest and equal-score suggestions must be ranked predictably",
            Arrays.asList(
                new PhSuggestions(
                    Arrays.asList(
                        "io.stdin",
                        "io.stdout",
                        "tt.sprintf",
                        "tt.trimmed",
                        "tt.concat"
                    )
                ).suggestions("Φ.io.std1out", 5).get(0),
                new PhSuggestions(
                    Arrays.asList("aa.aby", "aa.abx")
                ).suggestions("Φ.aa.abc", 5)
            ),
            Matchers.contains("io.stdout", Arrays.asList("aa.abx", "aa.aby"))
        );
    }

    @Test
    void limitsSuggestions() {
        MatcherAssert.assertThat(
            "Suggestion list must be limited",
            new PhSuggestions(
                Arrays.asList(
                    "io.stdout",
                    "io.stdouts",
                    "io.stdout-as-bytes",
                    "io.stdout-line",
                    "io.stdout-length",
                    "io.stdout-text"
                )
            ).suggestions("Φ.io.std1out", 5),
            Matchers.iterableWithSize(5)
        );
    }

    @Test
    void omitsMessageWithoutCandidates() {
        MatcherAssert.assertThat(
            "Suggestions must not render a section without candidates",
            Arrays.asList(
                new PhSuggestions(Collections.emptyList()).message("Φ.io.std1out"),
                new PhSuggestions(
                    Arrays.asList("zz.qqq", "aa.bbb", "unrelated.object")
                ).message("Φ.io.std1out")
            ),
            Matchers.contains("", "")
        );
    }

    @Test
    void preservesOrgEolangPrefix() {
        MatcherAssert.assertThat(
            "Suggestion must keep org.eolang prefix when the missing object uses it",
            new PhSuggestions(
                Arrays.asList("io.stdin", "io.stdout", "tt.sprintf")
            ).suggestions("Φ.org.eolang.io.std1out", 5).get(0),
            Matchers.equalTo("org.eolang.io.stdout")
        );
    }
}
