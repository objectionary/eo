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
 * Test case for {@link LnReversed}.
 * @since 0.1
 */
final class LnReversedTest {

    @Test
    void pushesBareReversedForVerticalForm() {
        final Stack stack = new Stack();
        new LnReversed(new Span("if. > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a `name.` with no horizontal args must push BARE_REVERSED",
            stack.top().kind(),
            Matchers.equalTo(Kind.BARE_REVERSED)
        );
    }

    @Test
    void leavesBareReversedOpen() {
        final Stack stack = new Stack();
        new LnReversed(new Span("if. > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "BARE_REVERSED must remain OPEN so the deeper receiver line can attach",
            stack.top().openness(),
            Matchers.equalTo(Openness.OPEN)
        );
    }

    @Test
    void pushesReversedWithHargsForHorizontalForm() {
        final Stack stack = new Stack();
        new LnReversed(new Span("if. cond then else > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a `name. arg1 arg2…` must push REVERSED_WITH_HARGS",
            stack.top().kind(),
            Matchers.equalTo(Kind.REVERSED_WITH_HARGS)
        );
    }

    @Test
    void marksHorizontalFormHorizontallyCompleted() {
        final Stack stack = new Stack();
        new LnReversed(new Span("if. cond then > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "REVERSED_WITH_HARGS must be HORIZONTAL_COMPLETED — no deeper continuation allowed",
            stack.top().openness(),
            Matchers.equalTo(Openness.HORIZONTAL_COMPLETED)
        );
    }

    @Test
    void emitsBaseWithLeadingDotAndNoMethodAttribute() {
        final Emit emit = new Emit();
        new LnReversed(new Span("if. > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a reversed dispatch opens a new chain (not a link), so it must emit <o base='.if'> without @method",
            LnReversedTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='x' and @base='.if' and not(@method)]"
            )
        );
    }

    @Test
    void emitsHorizontalArgsAsChildren() {
        final Emit emit = new Emit();
        new LnReversed(new Span("if. cond then else > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "horizontal form args must appear as children of the dispatch <o> in source order",
            LnReversedTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='x']/o[1][@base='cond']",
                "/object/o[@name='x']/o[2][@base='then']",
                "/object/o[@name='x']/o[3][@base='else']"
            )
        );
    }

    @Test
    void marksNamedWhenSuffixPresent() {
        final Stack stack = new Stack();
        new LnReversed(new Span("if. > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a reversed dispatch carrying a suffix must mark the pushed level as named",
            stack.top().named(),
            Matchers.is(true)
        );
    }

    /**
     * Render the emit's directives under a fresh {@code <object/>}.
     * @param emit The emit
     * @return XMIR
     */
    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
