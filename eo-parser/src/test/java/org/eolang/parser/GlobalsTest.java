/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

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
    void collapsesUnderIndentedBlankLineToEmpty() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 6);
        globals.appendTextLine("  ");
        MatcherAssert.assertThat(
            "a blank line shorter than the opener's indent must collapse to an empty line",
            globals.tbody(),
            Matchers.contains("")
        );
    }

    @Test
    void keepsSurplusSpacesOnIndentedBlankLine() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 2);
        globals.appendTextLine("    ");
        MatcherAssert.assertThat(
            "a blank line must retain spaces beyond the opener's indent",
            globals.tbody(),
            Matchers.contains("  ")
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

    @Test
    void keepsFlushedCommentsWhenTheSealingLineFails() {
        MatcherAssert.assertThat(
            "a top comment block flushed by a line that then fails must survive that line's rollback",
            GlobalsTest.render("# top doc", "", "  [x] > foo"),
            XhtmlMatchers.hasXPaths(
                "/object/comments/comment[contains(text(),'top doc')]",
                "/object[count(errors/error)=1]"
            )
        );
    }

    @Test
    void rollsTheSealBackWhenTheSealingLineFails() {
        MatcherAssert.assertThat(
            "a line failing after it sealed the header zone cannot turn the next comment line into an error",
            GlobalsTest.render("  [x] > foo", "# top doc", "", "[] > bar"),
            XhtmlMatchers.hasXPaths(
                "/object/comments/comment[contains(text(),'top doc')]",
                "/object[count(errors/error)=1]"
            )
        );
    }

    @Test
    void flushesTopBlockIntoComments() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# hello", 1));
        globals.blank();
        final Emit emit = new Emit();
        globals.seal(emit, new Span("+package foo", 3));
        MatcherAssert.assertThat(
            "the top comment block must flush into /object/comments when the header seals",
            GlobalsTest.render(emit),
            XhtmlMatchers.hasXPath("/object/comments/comment[contains(text(),'hello')]")
        );
    }

    @Test
    void reportsFirstLineOfMultiLineBlock() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# first", 1));
        globals.addComment(new Span("# second", 2));
        globals.blank();
        final Emit emit = new Emit();
        globals.seal(emit, new Span("+package foo", 4));
        MatcherAssert.assertThat(
            "a multi-line top comment block must report the line of its first span, not its last",
            GlobalsTest.render(emit),
            XhtmlMatchers.hasXPath("/object/comments/comment[@line='1']")
        );
    }

    @Test
    void clearsBufferAfterFlush() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# x", 1));
        globals.blank();
        globals.seal(new Emit(), new Span("[] > foo", 3));
        MatcherAssert.assertThat(
            "the comment buffer must be empty after the top block flushes",
            globals.pendingComments(),
            Matchers.empty()
        );
    }

    @Test
    void rejectsBlockWithoutTrailingBlank() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# doc", 1));
        Assertions.assertThrows(
            ParseError.class,
            () -> globals.seal(new Emit(), new Span("[] > foo", 2)),
            "a top comment block not followed by a blank line cannot be sealed"
        );
    }

    @Test
    void sealsHeaderZone() {
        final Globals globals = new Globals();
        globals.seal(new Emit(), new Span("+package foo", 1));
        MatcherAssert.assertThat(
            "the header zone must be sealed once the first meta or object lands",
            globals.sealed(),
            Matchers.is(true)
        );
    }

    @Test
    void doesNothingWhenAlreadySealed() {
        final Globals globals = new Globals();
        globals.seal();
        globals.addComment(new Span("# late", 1));
        final Emit emit = new Emit();
        globals.seal(emit, new Span("[] > foo", 2));
        MatcherAssert.assertThat(
            "a second seal cannot flush anything — the header is already closed",
            GlobalsTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/comments"))
        );
    }

    @Test
    void emitsNothingWhenBufferEmpty() {
        final Emit emit = new Emit();
        new Globals().seal(emit, new Span("[] > foo", 1));
        MatcherAssert.assertThat(
            "sealing with no pending comments cannot emit any comment element",
            GlobalsTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/comments"))
        );
    }

    private static String render(final String... rows) {
        final StringBuilder source = new StringBuilder(rows.length * 16);
        for (final String row : rows) {
            source.append(row).append((char) 10);
        }
        return new Xembler(
            new Directives().add("object").append(
                new Eo(source.toString()).directives()
            )
        ).xmlQuietly();
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
