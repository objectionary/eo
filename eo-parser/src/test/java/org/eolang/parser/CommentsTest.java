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
    void attachesCommentToNamedLineAtSameIndent() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# hello", 1));
        final Emit emit = new Emit();
        Comments.attach(globals, emit, new Span("[] > foo", 2), true);
        MatcherAssert.assertThat(
            "a comment at the same indent must flush to /object/comments when followed by a named line",
            CommentsTest.render(emit),
            XhtmlMatchers.hasXPath("/object/comments/comment[contains(text(),'hello')]")
        );
    }

    @Test
    void clearsBufferAfterAttachment() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# x", 1));
        Comments.attach(globals, new Emit(), new Span("[] > foo", 2), true);
        MatcherAssert.assertThat(
            "after flushing the comment buffer must be empty so the EOF check stays quiet",
            globals.pendingComments(),
            Matchers.empty()
        );
    }

    @Test
    void defersAttachmentWhenLineIsUnnamed() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# orphan", 1));
        Comments.attach(globals, new Emit(), new Span("foo", 2), false);
        MatcherAssert.assertThat(
            "an unnamed follower with no blank must leave the comment buffered for a"
                .concat(" later same-indent named line — no immediate error"),
            globals.pendingComments(),
            Matchers.hasSize(1)
        );
    }

    @Test
    void defersAttachmentWhenIndentMismatches() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# at-zero", 1));
        Comments.attach(globals, new Emit(), new Span("  [] > inner", 2), true);
        MatcherAssert.assertThat(
            "a deeper-indent named line with no intervening blank must not attach the"
                .concat(" buffered comment — it stays pending"),
            globals.pendingComments(),
            Matchers.hasSize(1)
        );
    }

    @Test
    void reportsDanglingWhenBlankLineIntervenes() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# split", 1));
        globals.blank();
        final Emit emit = new Emit();
        Comments.attach(globals, emit, new Span("[] > foo", 3), true);
        MatcherAssert.assertThat(
            "a blank line between comment and named line breaks attachment per R-6.5.2 —"
                .concat(" the comment must be reported as dangling and the buffer cleared"),
            CommentsTest.render(emit).contains("comment must precede a named object")
                && globals.pendingComments().isEmpty(),
            Matchers.is(true)
        );
    }

    @Test
    void doesNothingWhenBufferEmpty() {
        final Emit emit = new Emit();
        Comments.attach(new Globals(), emit, new Span("[] > foo", 1), true);
        MatcherAssert.assertThat(
            "with no pending comments the helper must not emit anything — directives stay empty",
            CommentsTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/*"))
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
