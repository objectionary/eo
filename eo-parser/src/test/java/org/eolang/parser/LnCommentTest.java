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
 * Test case for {@link LnComment}.
 * @since 0.1
 */
final class LnCommentTest {

    @Test
    void buffersSpanInPendingComments() {
        final Globals globals = new Globals();
        final Span span = new Span("# hello", 3);
        new LnComment(span).into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a comment line must append its span to the pending-comments buffer",
            globals.pendingComments(),
            Matchers.contains(span)
        );
    }

    @Test
    void resetsPendingBlankCounter() {
        final Globals globals = new Globals();
        globals.blank();
        new LnComment(new Span("# c", 2)).into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a non-blank line (including a comment) must clear pendingBlanks",
            globals.pendingBlanks(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void reportsDanglingWhenIndentChanges() {
        final Globals globals = new Globals();
        final Emit emit = new Emit();
        new LnComment(new Span("# first", 1)).into(new Stack(), globals, emit);
        new LnComment(new Span("  # second", 2)).into(new Stack(), globals, emit);
        MatcherAssert.assertThat(
            "two comment lines at different indents cannot share a block — the prior is dangling",
            LnCommentTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'comment must precede a named object')]"
            )
        );
    }

    @Test
    void startsFreshBlockAfterDangling() {
        final Globals globals = new Globals();
        final Emit emit = new Emit();
        new LnComment(new Span("# first", 1)).into(new Stack(), globals, emit);
        new LnComment(new Span("  # second", 2)).into(new Stack(), globals, emit);
        MatcherAssert.assertThat(
            "after dangling-report, the buffer must contain only the new comment",
            globals.pendingComments(),
            Matchers.hasSize(1)
        );
    }

    @Test
    void reportsDanglingWhenBlankLineSplitsBlock() {
        final Globals globals = new Globals();
        final Emit emit = new Emit();
        new LnComment(new Span("# first", 1)).into(new Stack(), globals, emit);
        globals.blank();
        new LnComment(new Span("# second", 3)).into(new Stack(), globals, emit);
        MatcherAssert.assertThat(
            "a blank line between two comment lines must split them and report the prior block as dangling",
            LnCommentTest.render(emit),
            XhtmlMatchers.hasXPath("/object/errors/error")
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
