/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Globals}.
 * @since 0.1
 */
final class GlobalsTest {

    @Test
    void startsWithNoObjectEmitted() {
        MatcherAssert.assertThat(
            "a fresh Globals cannot have any object marked as emitted yet",
            new Globals().firstObjectEmitted(),
            Matchers.is(false)
        );
    }

    @Test
    void flipsFirstObjectEmitted() {
        final Globals globals = new Globals();
        globals.markEmitted();
        MatcherAssert.assertThat(
            "firstObjectEmitted must report true once markEmitted has been called",
            globals.firstObjectEmitted(),
            Matchers.is(true)
        );
    }

    @Test
    void countsConsecutiveBlankLines() {
        final Globals globals = new Globals();
        globals.blank();
        globals.blank();
        MatcherAssert.assertThat(
            "pendingBlanks must equal the number of blank() invocations since the last clear",
            globals.pendingBlanks(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void resetsPendingBlanksWhenCleared() {
        final Globals globals = new Globals();
        globals.blank();
        globals.clearBlanks();
        MatcherAssert.assertThat(
            "clearBlanks must zero the pending-blank counter",
            globals.pendingBlanks(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void tracksTrailingBlanksAcrossBlanks() {
        final Globals globals = new Globals();
        globals.blank();
        globals.blank();
        MatcherAssert.assertThat(
            "trailingBlanks must increment alongside the pending counter",
            globals.trailingBlanks(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void resetsTrailingBlanksOnClear() {
        final Globals globals = new Globals();
        globals.blank();
        globals.clearBlanks();
        MatcherAssert.assertThat(
            "clearBlanks must also zero the trailing counter so EOF only sees the actual tail",
            globals.trailingBlanks(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void recordsTextBlockOpenLine() {
        final Globals globals = new Globals();
        globals.openTextBlock(17);
        MatcherAssert.assertThat(
            "textBlockOpenLine must round-trip the opener line for the unclosed-text-block error",
            globals.textBlockOpenLine(),
            Matchers.equalTo(17)
        );
    }

    @Test
    void closesTextBlockState() {
        final Globals globals = new Globals();
        globals.openTextBlock(3);
        globals.closeTextBlock();
        MatcherAssert.assertThat(
            "closeTextBlock must drop the in-text flag back to false",
            globals.inTextBlock(),
            Matchers.is(false)
        );
    }

    @Test
    void exposesBufferedCommentInPending() {
        final Globals globals = new Globals();
        final Span span = new Span("# hi", 1);
        globals.addComment(span);
        MatcherAssert.assertThat(
            "addComment must append the span to the pending buffer in source order",
            globals.pendingComments(),
            Matchers.contains(span)
        );
    }

    @Test
    void clearsPendingComments() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# hi", 1));
        globals.clearComments();
        MatcherAssert.assertThat(
            "clearComments must drop all buffered comments so attachment cannot fire twice",
            globals.pendingComments(),
            Matchers.empty()
        );
    }
}
