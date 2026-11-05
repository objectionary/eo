/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Label}.
 *
 * @since 0.76.0
 */
final class LabelTest {

    @Test
    void saysAVoidNobodyFillsInWords() {
        MatcherAssert.assertThat(
            "a void nobody fills must be said in words, but its mark came through",
            new Label("?unknown-empty").words(),
            Matchers.equalTo("unknown, filled by nobody")
        );
    }

    @Test
    void saysAVoidOfAnObjectNobodyCallsInWords() {
        MatcherAssert.assertThat(
            "a void of an object nobody calls must be said in words, but its mark came through",
            new Label("?unknown-study").words(),
            Matchers.equalTo("unknown, nobody calls this object")
        );
    }

    @Test
    void saysAnUnknownAtomFillsInWords() {
        MatcherAssert.assertThat(
            "an unknown an atom fills must be said in words, but its mark came through",
            new Label("?unknown-java").words(),
            Matchers.equalTo("unknown, filled by an atom")
        );
    }

    @Test
    void saysAReadNothingAnswersInWords() {
        MatcherAssert.assertThat(
            "a read nothing ever answers must be said in words, but its mark came through",
            new Label("?unknown-loop").words(),
            Matchers.equalTo("unknown, nothing ever answers")
        );
    }

    @Test
    void spellsAnUnknownMarkWithSpaces() {
        MatcherAssert.assertThat(
            "a mark with no words of its own must be spelled with spaces, but it wasnt",
            new Label("?end-nobody-fills-it").words(),
            Matchers.equalTo("end nobody fills it")
        );
    }

    @Test
    void drawsALocatorAsAName() {
        MatcherAssert.assertThat(
            "a locator must be drawn as a name, but it wasnt",
            new Label("Φ.bytes.as-number").kind(),
            Matchers.equalTo("name")
        );
    }

    @Test
    void saysTypeTreeAsItIsWritten() {
        MatcherAssert.assertThat(
            "a type tree must be said as it is written, but it was changed",
            new Label("Φ.posix.status-return(Φ.number,Φ.bytes|Φ.string)").words(),
            Matchers.equalTo("Φ.posix.status-return(Φ.number,Φ.bytes|Φ.string)")
        );
    }
}
