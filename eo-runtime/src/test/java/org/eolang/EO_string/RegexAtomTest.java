/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.eolang.Bind;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests for the regex atom.
 * @since 0.57.4
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class RegexAtomTest {

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

    @Test
    void rejectsStartIndexOutOfIntRange() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a %s index outside int range",
                RegexAtomTest.start()
            ),
            RegexAtomTest.rejection(new Data.ToPhi(1.0e15)),
            Matchers.allOf(
                Matchers.containsString(RegexAtomTest.start()),
                Matchers.containsString("must fit into int range")
            )
        );
    }

    @Test
    void rejectsFractionalStartIndex() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a fractional %s index",
                RegexAtomTest.start()
            ),
            RegexAtomTest.rejection(new Data.ToPhi(2.7)),
            Matchers.allOf(
                Matchers.containsString(RegexAtomTest.start()),
                Matchers.containsString("must be an integer")
            )
        );
    }

    @Test
    void rejectsNegativeStartIndex() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a negative %s index cleanly",
                RegexAtomTest.start()
            ),
            RegexAtomTest.rejection(new Data.ToPhi(-1)),
            Matchers.allOf(
                Matchers.containsString(RegexAtomTest.start()),
                Matchers.containsString("must be greater or equal to zero")
            )
        );
    }

    @Test
    void readsFromWhenOptionalGroupDoesNotParticipate() {
        final Phi matched = RegexAtomTest.optionalGroupMatch();
        MatcherAssert.assertThat(
            "match with a non-participating optional group must not crash when reading from",
            new Dataized(matched.take("from")).asNumber(),
            Matchers.equalTo(0.0)
        );
        MatcherAssert.assertThat(
            "non-participating optional capture must be an empty string, not absent",
            new Dataized(
                new PhApplication(
                    matched.take("group").copy(),
                    new Bind("index", new Data.ToPhi(2))
                )
            ).asString(),
            Matchers.equalTo("")
        );
        MatcherAssert.assertThat(
            "group slots must stay aligned with groupCount+1 even when a group does not participate",
            new Dataized(matched.take("count")).asNumber(),
            Matchers.equalTo(3.0)
        );
    }

    @Test
    void rejectsStartIndexAfterTextEnd() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a %s index after text end cleanly",
                RegexAtomTest.start()
            ),
            RegexAtomTest.rejection(new Data.ToPhi(6)),
            Matchers.allOf(
                Matchers.containsString(RegexAtomTest.start()),
                Matchers.containsString("must be less than or equal to text length")
            )
        );
    }

    /**
     * Build matched-from-index for /(a)(b)?/ against "a".
     * @return Matched block
     */
    private static Phi optionalGroupMatch() {
        return new PhApplication(
            new PhApplication(
                new PhApplication(
                    Phi.Φ.take("string.regex").copy(),
                    "expression", new Data.ToPhi("/(a)(b)?/")
                ).take("compiled").take("match").copy(),
                "txt", new Data.ToPhi("a")
            ).take("matched-from-index").copy(),
            new Bind(RegexAtomTest.position(), new Data.ToPhi(1)),
            new Bind(RegexAtomTest.start(), new Data.ToPhi(0))
        );
    }

    /**
     * Dataize matched-from-index and return its rejection message.
     * @param start Start index
     * @return Rejection message
     */
    private static String rejection(final Phi start) {
        return Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(RegexAtomTest.matchedFromIndex(start).take("from")).take(),
            "start index must be rejected before Matcher.find(int)"
        ).toString();
    }

    /**
     * Build an internal matched-from-index application.
     * @param start Start index
     * @return Application
     */
    private static Phi matchedFromIndex(final Phi start) {
        return new PhApplication(
            new PhApplication(
                new PhApplication(
                    Phi.Φ.take("string.regex").copy(),
                    "expression", new Data.ToPhi("/[a-z]+/")
                ).take("compiled").take("match").copy(),
                "txt", new Data.ToPhi("hello")
            ).take("matched-from-index").copy(),
            new Bind(RegexAtomTest.position(), new Data.ToPhi(1)),
            new Bind(RegexAtomTest.start(), start)
        );
    }

    /**
     * Start attribute name.
     * @return Start attribute name
     */
    private static String start() {
        return "start";
    }

    /**
     * Position attribute name.
     * @return Position attribute name
     */
    private static String position() {
        return "position";
    }
}
