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
            "an open void must be said in words, but its mark came through",
            new Label("?open-void").words(),
            Matchers.equalTo("an open void, filled by nobody")
        );
    }

    @Test
    void drawsAFailingReadAsAFailure() {
        MatcherAssert.assertThat(
            "a read that fails must be drawn as a failure, but it wasnt",
            new Label("!Φ.posix.status-return.name").kind(),
            Matchers.equalTo("fails")
        );
    }

    @Test
    void namesTheReadThatFails() {
        MatcherAssert.assertThat(
            "a failing read must be said by the name it fails on, but it wasnt",
            new Label("!Φ.posix.status-return.name").words(),
            Matchers.equalTo("Φ.posix.status-return.name")
        );
    }

    @Test
    void drawsTheCaretOnlyJavaFillsInWords() {
        MatcherAssert.assertThat(
            "a caret only Java fills has words of its own, but it was drawn as a failing read",
            new Label("!only-java-fills-ρ").kind(),
            Matchers.equalTo("words")
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
    void drawsASkippedPlaceAsSkipped() {
        MatcherAssert.assertThat(
            "a place the engine skipped must be drawn as skipped, but it wasnt",
            new Label("?skipped-regex").kind(),
            Matchers.equalTo("skipped")
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
}
