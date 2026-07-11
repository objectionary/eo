/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Comments}.
 * @since 0.1
 */
final class CommentsTest {

    @Test
    void flushesTopBlockIntoComments() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# hello", 1));
        final Emit emit = new Emit();
        Comments.seal(globals, emit, new Span("+package foo", 2));
        MatcherAssert.assertThat(
            "the top comment block must flush into /object/comments when the header seals",
            CommentsTest.render(emit),
            XhtmlMatchers.hasXPath("/object/comments/comment[contains(text(),'hello')]")
        );
    }

    @Test
    void clearsBufferAfterFlush() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# x", 1));
        Comments.seal(globals, new Emit(), new Span("[] > foo", 2));
        MatcherAssert.assertThat(
            "the comment buffer must be empty after the top block flushes",
            globals.pendingComments(),
            Matchers.empty()
        );
    }

    @Test
    void sealsHeaderZone() {
        final Globals globals = new Globals();
        Comments.seal(globals, new Emit(), new Span("+package foo", 1));
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
        Comments.seal(globals, emit, new Span("[] > foo", 2));
        MatcherAssert.assertThat(
            "a second seal cannot flush anything — the header is already closed",
            CommentsTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/comments"))
        );
    }

    @Test
    void emitsNothingWhenBufferEmpty() {
        final Emit emit = new Emit();
        Comments.seal(new Globals(), emit, new Span("[] > foo", 1));
        MatcherAssert.assertThat(
            "sealing with no pending comments cannot emit any comment element",
            CommentsTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/comments"))
        );
    }

    /**
     * Render the emit's directives under a fresh {@code <object/>}.
     * @param emit The emit
     * @return XMIR document
     */
    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
