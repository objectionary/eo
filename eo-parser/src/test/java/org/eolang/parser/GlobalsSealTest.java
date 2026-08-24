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
 * Test case for {@link Globals#seal(Emit, Span)}.
 * @since 0.1
 */
final class GlobalsSealTest {

    @Test
    void flushesTopBlockIntoComments() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# hello", 1));
        globals.blank();
        final Emit emit = new Emit();
        globals.seal(emit, new Span("+package foo", 3));
        MatcherAssert.assertThat(
            "the top comment block must flush into /object/comments when the header seals",
            GlobalsSealTest.render(emit),
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
            GlobalsSealTest.render(emit),
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
            GlobalsSealTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/comments"))
        );
    }

    @Test
    void emitsNothingWhenBufferEmpty() {
        final Emit emit = new Emit();
        new Globals().seal(emit, new Span("[] > foo", 1));
        MatcherAssert.assertThat(
            "sealing with no pending comments cannot emit any comment element",
            GlobalsSealTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/comments"))
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
