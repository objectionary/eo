/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOstring$EOregex$EOpattern$EOchecked}.
 * @since 0.77
 */
final class EOstringEOregexEOpatternEOcheckedTest {

    @Test
    void handsThePatternBackWhenTheSourceCompiles() {
        MatcherAssert.assertThat(
            "a source that compiles must hand back the very pattern it was checked on, so that the caller goes on matching with it",
            new Dataized(
                EOstringEOregexEOpatternEOcheckedTest.pattern("[a-z]+", "i")
                    .take("checked").take("source")
            ).asString(),
            Matchers.equalTo("(?i)[a-z]+")
        );
    }

    @Test
    void reportsTheConstructAndTheIndexOfAnInvalidSource() {
        MatcherAssert.assertThat(
            "an invalid source must hand the fallback the construct the engine choked on and the index it sits at",
            EOstringEOregexEOpatternEOcheckedTest.report("a**", ""),
            Matchers.equalTo("Dangling meta character '*' at 2")
        );
    }

    @Test
    void countsTheIndexFromTheStartOfTheSplicedSource() {
        MatcherAssert.assertThat(
            "the index must be the engine's own, into the source with the flags spliced in front, since regex.eo takes the flag group off itself",
            EOstringEOregexEOpatternEOcheckedTest.report("a**", "i"),
            Matchers.equalTo("Dangling meta character '*' at 6")
        );
    }

    @Test
    void terminatesWithTheConstructWhenNoFallbackIsBound() {
        MatcherAssert.assertThat(
            "an invalid source with no fallback bound must terminate with the construct the engine choked on, not with an opaque reason",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    EOstringEOregexEOpatternEOcheckedTest.pattern("(", "").take("checked")
                ).take()
            ).toString(),
            Matchers.containsString("Unclosed group")
        );
    }

    private static String report(final String body, final String flags) {
        final Phi checked = EOstringEOregexEOpatternEOcheckedTest.pattern(body, flags)
            .take("checked").copy();
        checked.put(0, new EOstringEOregexEOpatternEOcheckedTest.Report());
        return new Dataized(checked).asString();
    }

    private static Phi pattern(final String body, final String flags) {
        final Phi pattern = Phi.Φ.take("string").take("regex").take("pattern").copy();
        pattern.put(0, new Data.ToPhi(body));
        pattern.put(1, new Data.ToPhi(flags));
        return pattern;
    }

    /**
     * A fallback that renders the reason and the index it is given.
     * [reason index] > report
     * "%s at %d".printf (* reason index) > @
     * @since 0.77
     */
    private static final class Report extends PhDefault implements Atom {

        /**
         * Ctor.
         */
        Report() {
            super(
                new Attrs(
                    new Attr("reason", new AtVoid("reason")),
                    new Attr("index", new AtVoid("index"))
                )
            );
        }

        @Override
        public Phi lambda() {
            return new Data.ToPhi(
                String.format(
                    "%s at %d",
                    new Dataized(this.take("reason")).asString(),
                    new Dataized(this.take("index")).asNumber().intValue()
                )
            );
        }
    }
}
