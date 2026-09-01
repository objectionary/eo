/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Blanks}.
 * @since 0.1
 */
final class BlanksTest {

    @Test
    void reportsMissingBlankBeforeAttributeThroughGlobals() {
        final Emit emit = new Emit();
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        Blanks.checkTest(new Span("  +> foo", 2), stack, new Globals(), emit);
        MatcherAssert.assertThat(
            "checkTest must read the blank count from Globals and report a missing blank",
            BlanksTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error[contains(text(),'missing blank line')]"
            )
        );
    }

    @Test
    void staysSilentAboutAttributeBlankWhenGlobalsHasPendingBlanks() {
        final Emit emit = new Emit();
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        final Globals globals = new Globals();
        globals.blank();
        Blanks.checkTest(new Span("  +> foo", 2), stack, globals, emit);
        MatcherAssert.assertThat(
            "checkTest must not report a missing blank once Globals carries a pending one",
            BlanksTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object[not(errors/error[contains(text(),'missing blank line')])]"
            )
        );
    }

    @Test
    void keepsBlankAfterTopCommentBlock() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# top doc", 1));
        globals.blank();
        final Emit emit = new Emit();
        Blanks.checkPlain(new Span("I > foo", 3), globals, emit);
        MatcherAssert.assertThat(
            "the mandatory blank after a top comment must not become a sibling blank",
            BlanksTest.render(emit),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
