/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link LnComment}.
 * @since 0.1
 */
final class LnCommentTest {

    @Test
    void buffersTopCommentSpan() {
        final Globals globals = new Globals();
        final Span span = new Span("# hello", 3);
        new LnComment(span).into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a top comment line must append its span to the pending-comments buffer",
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
    void rejectsCommentAfterHeaderSealed() {
        final Globals globals = new Globals();
        globals.seal();
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnComment(new Span("# late", 7)).into(new Stack(), globals, new Emit()),
            "a comment after the first meta or object cannot be accepted"
        );
    }

    @Test
    void rejectsIndentedComment() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnComment(
                new Span("  # inner", 4)
            ).into(new Stack(), new Globals(), new Emit()),
            "an indented comment cannot sit in the top block"
        );
    }

    @Test
    void rejectsBlankInsideBlock() {
        final Globals globals = new Globals();
        new LnComment(new Span("# first", 1)).into(new Stack(), globals, new Emit());
        globals.blank();
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnComment(new Span("# second", 3)).into(new Stack(), globals, new Emit()),
            "a blank line cannot split the top comment block into two"
        );
    }
}
