/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;
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
 * Test case for {@link EOregex$EOpattern$EOmatch$EOmatched_from_index}.
 * @since 0.57.4
 */
final class EOregexEOpatternEOmatchEOmatchedfromindexTest {

    @Test
    void rejectsStartIndexOutOfIntRange() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a %s index outside int range",
                EOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(1.0e15)),
            Matchers.allOf(
                Matchers.containsString(EOregexEOpatternEOmatchEOmatchedfromindexTest.start()),
                Matchers.containsString("must fit into int range")
            )
        );
    }

    @Test
    void rejectsFractionalStartIndex() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a fractional %s index",
                EOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(2.7)),
            Matchers.allOf(
                Matchers.containsString(EOregexEOpatternEOmatchEOmatchedfromindexTest.start()),
                Matchers.containsString("must be an integer")
            )
        );
    }

    @Test
    void rejectsNegativeStartIndex() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a negative %s index cleanly",
                EOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(-1)),
            Matchers.allOf(
                Matchers.containsString(EOregexEOpatternEOmatchEOmatchedfromindexTest.start()),
                Matchers.containsString("must be greater or equal to zero")
            )
        );
    }

    @Test
    void readsFromWhenOptionalGroupDoesNotParticipate() {
        MatcherAssert.assertThat(
            "match with a non-participating optional group must not crash when reading from",
            new Dataized(
                EOregexEOpatternEOmatchEOmatchedfromindexTest
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
                    EOregexEOpatternEOmatchEOmatchedfromindexTest
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
                EOregexEOpatternEOmatchEOmatchedfromindexTest
                    .optionalGroupMatch().take("count")
            ).asNumber(),
            Matchers.equalTo(3.0)
        );
    }

    @Test
    void rejectsStartIndexAfterTextEnd() {
        MatcherAssert.assertThat(
            String.format(
                "matched-from-index must reject a %s index after text end cleanly",
                EOregexEOpatternEOmatchEOmatchedfromindexTest.start()
            ),
            EOregexEOpatternEOmatchEOmatchedfromindexTest.rejection(new Data.ToPhi(6)),
            Matchers.allOf(
                Matchers.containsString(EOregexEOpatternEOmatchEOmatchedfromindexTest.start()),
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
        final Phi pattern = Phi.Φ.take("string.regex").take("pattern").copy();
        pattern.put(0, new Data.ToPhi(baos.toByteArray()));
        final ExAbstract failure = Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                new PhApplication(
                    new PhApplication(
                        pattern.take("match").copy(),
                        "txt", new Data.ToPhi("hello")
                    ).take("matched-from-index").copy(),
                    new Bind(EOregexEOpatternEOmatchEOmatchedfromindexTest.position(), new Data.ToPhi(1)),
                    new Bind(EOregexEOpatternEOmatchEOmatchedfromindexTest.start(), new Data.ToPhi(0))
                ).take("from")
            ).take(),
            "bytes deserializing to the wrong type must fail with ExFailure, not a raw ClassCastException"
        );
        MatcherAssert.assertThat(
            "the failure must be the clean deserialize message, not a raw ClassCastException",
            failure.toString(),
            Matchers.containsString("cannot deserialize the compiled regex pattern")
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
            new Bind(EOregexEOpatternEOmatchEOmatchedfromindexTest.position(), new Data.ToPhi(1)),
            new Bind(EOregexEOpatternEOmatchEOmatchedfromindexTest.start(), new Data.ToPhi(0))
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
            () -> new Dataized(
                EOregexEOpatternEOmatchEOmatchedfromindexTest
                    .matchedFromIndex(start).take("from")
            ).take(),
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
            new Bind(EOregexEOpatternEOmatchEOmatchedfromindexTest.position(), new Data.ToPhi(1)),
            new Bind(EOregexEOpatternEOmatchEOmatchedfromindexTest.start(), start)
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
