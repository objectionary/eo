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
 * Test case for {@link EOstring$EOregex$EOpattern$EOmatch$EOsearched}.
 * @since 0.77
 */
final class EOstringEOregexEOpatternEOmatchEOsearchedTest {

    @Test
    void answersAnEmptyTupleWhenNothingMatches() {
        MatcherAssert.assertThat(
            "a text without a match must be answered with an empty tuple, since that is how regex.eo tells the missing block apart",
            new Dataized(
                EOstringEOregexEOpatternEOmatchEOsearchedTest
                    .searched("/[0-9]+/", "hello", new Data.ToPhi(0))
                    .take("length")
            ).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void answersTheBordersOfTheFirstBlockAfterTheStart() {
        final Phi block = EOstringEOregexEOpatternEOmatchEOsearchedTest.searched(
            "/[a-z]+/", "!hello!world!", new Data.ToPhi(7)
        );
        MatcherAssert.assertThat(
            "the block must be the first one at or after the start, with its borders as the first two elements",
            new double[]{
                new Dataized(
                    EOstringEOregexEOpatternEOmatchEOsearchedTest.element(block, 0)
                ).asNumber(),
                new Dataized(
                    EOstringEOregexEOpatternEOmatchEOsearchedTest.element(block, 1)
                ).asNumber(),
            },
            Matchers.equalTo(new double[]{7.0, 12.0})
        );
    }

    @Test
    void countsASupplementaryCharacterAsOnePosition() {
        MatcherAssert.assertThat(
            "a supplementary character before the block must count as one position, not as the two UTF-16 units it takes",
            new Dataized(
                EOstringEOregexEOpatternEOmatchEOsearchedTest.element(
                    EOstringEOregexEOpatternEOmatchEOsearchedTest.searched(
                        "/x/", "😀x", new Data.ToPhi(0)
                    ),
                    0
                )
            ).asNumber(),
            Matchers.equalTo(1.0)
        );
    }

    @Test
    void answersAnEmptyStringForANonParticipatingGroup() {
        MatcherAssert.assertThat(
            "a group that did not participate must be an empty string, not absent, so that the group slots stay aligned",
            new Dataized(
                EOstringEOregexEOpatternEOmatchEOsearchedTest.element(
                    EOstringEOregexEOpatternEOmatchEOsearchedTest.element(
                        EOstringEOregexEOpatternEOmatchEOsearchedTest.optionalGroup(), 2
                    ),
                    2
                )
            ).asString(),
            Matchers.equalTo("")
        );
    }

    @Test
    void tellsWhichGroupsParticipated() {
        final Phi existing = EOstringEOregexEOpatternEOmatchEOsearchedTest.element(
            EOstringEOregexEOpatternEOmatchEOsearchedTest.optionalGroup(), 3
        );
        MatcherAssert.assertThat(
            "the fourth element must say true for a group that participated and false for one that did not",
            new boolean[]{
                new Dataized(
                    EOstringEOregexEOpatternEOmatchEOsearchedTest.element(existing, 1)
                ).asBool(),
                new Dataized(
                    EOstringEOregexEOpatternEOmatchEOsearchedTest.element(existing, 2)
                ).asBool(),
            },
            Matchers.equalTo(new boolean[]{true, false})
        );
    }

    @Test
    void rejectsStartIndexOutOfIntRange() {
        MatcherAssert.assertThat(
            "searched must reject a start index outside int range",
            EOstringEOregexEOpatternEOmatchEOsearchedTest.rejection(new Data.ToPhi(1.0e15)),
            Matchers.allOf(
                Matchers.containsString(EOstringEOregexEOpatternEOmatchEOsearchedTest.start()),
                Matchers.containsString("must fit into int range")
            )
        );
    }

    @Test
    void rejectsFractionalStartIndex() {
        MatcherAssert.assertThat(
            "searched must reject a fractional start index",
            EOstringEOregexEOpatternEOmatchEOsearchedTest.rejection(new Data.ToPhi(2.7)),
            Matchers.allOf(
                Matchers.containsString(EOstringEOregexEOpatternEOmatchEOsearchedTest.start()),
                Matchers.containsString("must be an integer")
            )
        );
    }

    @Test
    void rejectsNegativeStartIndex() {
        MatcherAssert.assertThat(
            "searched must reject a negative start index cleanly",
            EOstringEOregexEOpatternEOmatchEOsearchedTest.rejection(new Data.ToPhi(-1)),
            Matchers.allOf(
                Matchers.containsString(EOstringEOregexEOpatternEOmatchEOsearchedTest.start()),
                Matchers.containsString("must be greater or equal to zero")
            )
        );
    }

    @Test
    void rejectsStartIndexAfterTextEnd() {
        MatcherAssert.assertThat(
            "searched must reject a start index after the text end cleanly",
            EOstringEOregexEOpatternEOmatchEOsearchedTest.rejection(new Data.ToPhi(6)),
            Matchers.allOf(
                Matchers.containsString(EOstringEOregexEOpatternEOmatchEOsearchedTest.start()),
                Matchers.containsString("must be less than or equal to text length")
            )
        );
    }

    @Test
    void stopsOnASourceThatDoesNotCompile() {
        final Phi pattern = Phi.Φ.take("string").take("regex").take("pattern").copy();
        pattern.put(0, new Data.ToPhi("[a-z"));
        pattern.put(1, new Data.ToPhi(""));
        MatcherAssert.assertThat(
            "a pattern built straight from a body that does not compile must fail with a clean reason, not with a raw engine exception",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            pattern.take("match").copy(),
                            "txt", new Data.ToPhi("hello")
                        ).take("searched").copy(),
                        EOstringEOregexEOpatternEOmatchEOsearchedTest.start(),
                        new Data.ToPhi(0)
                    ).take("length")
                ).take()
            ).toString(),
            Matchers.containsString("cannot search with the regex pattern '[a-z'")
        );
    }

    private static Phi optionalGroup() {
        return EOstringEOregexEOpatternEOmatchEOsearchedTest.searched(
            "/(a)(b)?/", "a", new Data.ToPhi(0)
        );
    }

    private static String rejection(final Phi start) {
        return Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                EOstringEOregexEOpatternEOmatchEOsearchedTest
                    .searched("/[a-z]+/", "hello", start)
                    .take("length")
            ).take(),
            "start index must be rejected before Matcher.find(int)"
        ).toString();
    }

    private static Phi searched(final String expression, final String text, final Phi start) {
        return new PhApplication(
            new PhApplication(
                new Data.ToPhi(expression)
                    .take("regex").take("compiled").take("match").copy(),
                "txt", new Data.ToPhi(text)
            ).take("searched").copy(),
            EOstringEOregexEOpatternEOmatchEOsearchedTest.start(),
            start
        );
    }

    private static String start() {
        return "start";
    }

    private static Phi element(final Phi tuple, final int index) {
        return new PhApplication(tuple.take("at").copy(), "i", new Data.ToPhi(index));
    }
}
