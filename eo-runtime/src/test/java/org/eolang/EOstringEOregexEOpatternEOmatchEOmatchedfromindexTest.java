/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOstring$EOregex$EOpattern$EOmatch$EOmatched_from_index}.
 * @since 0.57.4
 */
final class EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest {

    @Test
    void rejectsStartIndexOutOfIntRange() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a %s index outside int range",
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(1.0e15)),
            Matchers.allOf(
                Matchers.containsString(
                    EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
                ),
                Matchers.containsString("must fit into int range")
            )
        );
    }

    @Test
    void rejectsFractionalStartIndex() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a fractional %s index",
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(2.7)),
            Matchers.allOf(
                Matchers.containsString(
                    EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
                ),
                Matchers.containsString("must be an integer")
            )
        );
    }

    @Test
    void rejectsNegativeStartIndex() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a negative %s index cleanly",
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(-1)),
            Matchers.allOf(
                Matchers.containsString(
                    EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
                ),
                Matchers.containsString("must be greater or equal to zero")
            )
        );
    }

    @Test
    void readsFromWhenOptionalGroupDoesNotParticipate() {
        MatcherAssert.assertThat(
            "match with a non-participating optional group must not crash when reading from",
            new Dataized(
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest
                    .optionalGroupMatch().take("from")
            ).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void readsEmptyStringForNonParticipatingOptionalGroup() {
        MatcherAssert.assertThat(
            "non-participating optional capture must be an empty string, not absent",
            new Dataized(
                new PhApplication(
                    EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest
                        .optionalGroupMatch().take("group").copy(),
                    new Bind("index", new Data.ToPhi(2))
                )
            ).asString(),
            Matchers.equalTo("")
        );
    }

    @Test
    void keepsGroupSlotsAlignedWhenOptionalGroupDoesNotParticipate() {
        MatcherAssert.assertThat(
            "group slots must stay aligned with groupCount+1 even when a group does not participate",
            new Dataized(
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest
                    .optionalGroupMatch().take("count")
            ).asNumber(),
            Matchers.equalTo(3.0)
        );
    }

    @Test
    void readsGroupExistsAsFalseForNonParticipatingOptionalGroup() {
        MatcherAssert.assertThat(
            "group-exists must be false for an optional group that did not participate",
            new Dataized(
                new PhApplication(
                    EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest
                        .optionalGroupMatch().take("group-exists").copy(),
                    new Bind("index", new Data.ToPhi(2))
                )
            ).asBool(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void readsGroupExistsAsTrueForParticipatingGroup() {
        MatcherAssert.assertThat(
            "group-exists must be true for a group that did participate",
            new Dataized(
                new PhApplication(
                    EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest
                        .optionalGroupMatch().take("group-exists").copy(),
                    new Bind("index", new Data.ToPhi(1))
                )
            ).asBool(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void rejectsStartIndexAfterTextEnd() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a %s index after text end cleanly",
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(6)),
            Matchers.allOf(
                Matchers.containsString(
                    EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start()
                ),
                Matchers.containsString("must be less than or equal to text length")
            )
        );
    }

    @Test
    void rejectsSerializedBytesOfTheWrongType() throws Exception {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(baos)) {
            oos.writeObject("not a pattern");
        }
        final Phi pattern = new Data.ToPhi("/x/").take("regex").take("pattern").copy();
        pattern.put(0, new Data.ToPhi(baos.toByteArray()));
        MatcherAssert.assertThat(
            "a raw ClassCastException leaked instead of the clean deserialize failure",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            pattern.take("match").copy(),
                            "txt", new Data.ToPhi("hello")
                        ).take("matched-from-index").copy(),
                        new Bind(
                            EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.position(),
                            new Data.ToPhi(1)
                        ),
                        new Bind(
                            EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start(),
                            new Data.ToPhi(0)
                        )
                    ).take("from")
                ).take(),
                "bytes of the wrong type must fail with ExFailure"
            ).toString(),
            Matchers.containsString("cannot deserialize the compiled regex pattern")
        );
    }

    private static Phi optionalGroupMatch() {
        return new PhApplication(
            new PhApplication(
                new Data.ToPhi("/(a)(b)?/")
                    .take("regex").take("compiled").take("match").copy(),
                "txt", new Data.ToPhi("a")
            ).take("matched-from-index").copy(),
            new Bind(
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.position(),
                new Data.ToPhi(1)
            ),
            new Bind(
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start(),
                new Data.ToPhi(0)
            )
        );
    }

    private static String rejection(final Phi start) {
        return Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest
                    .matchedFromIndex(start).take("from")
            ).take(),
            "start index must be rejected before Matcher.find(int)"
        ).toString();
    }

    private static Phi matchedFromIndex(final Phi start) {
        return new PhApplication(
            new PhApplication(
                new Data.ToPhi("/[a-z]+/")
                    .take("regex").take("compiled").take("match").copy(),
                "txt", new Data.ToPhi("hello")
            ).take("matched-from-index").copy(),
            new Bind(
                EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.position(),
                new Data.ToPhi(1)
            ),
            new Bind(EOstringEOregexEOpatternEOmatchEOmatchedfromindexTest.start(), start)
        );
    }

    private static String start() {
        return "start";
    }

    private static String position() {
        return "position";
    }
}
