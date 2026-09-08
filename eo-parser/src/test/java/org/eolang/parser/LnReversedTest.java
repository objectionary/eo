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
 *
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
            "a `name. arg1 arg2…` must push REVERSED_HARGS",
            stack.top().kind(),
            Matchers.equalTo(Kind.REVERSED_HARGS)
        );
    }

    @Test
    void marksHorizontalFormHorizontallyCompleted() {
        final Stack stack = new Stack();
        new LnReversed(new Span("if. cond then > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "REVERSED_HARGS must be HCOMPLETED — no deeper continuation allowed",
            stack.top().openness(),
            Matchers.equalTo(Openness.HCOMPLETED)
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

    @Test
    void emitsLocalHandleForAutoSuffix() {
        final Emit emit = new Emit();
        new LnReversed(new Span("if. >> index", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `>> name` auto suffix on a reversed dispatch must emit the file-local handle as @local so resolve-local-names.xsl can see it (#5874)",
            LnReversedTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@local='index' and @base='.if' and not(@method)]"
            )
        );
    }

    @Test
    void reportsRootHeadColumnWithLineIndent() {
        final Span span = new Span("    @. > x", 3);
        MatcherAssert.assertThat(
            "a root head's pos() must count the line's indent, not just the stripped-body offset",
            LnReversed.readHead(new Tokens(span.body(), span), span.indent()).pos(),
            Matchers.equalTo(4)
        );
    }

    @Test
    void reportsRhoHeadColumnWithLineIndent() {
        final Span span = new Span("  ^. > x", 3);
        MatcherAssert.assertThat(
            "a `^.` root head's pos() must also count the line's indent",
            LnReversed.readHead(new Tokens(span.body(), span), span.indent()).pos(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void reportsXiHeadColumnWithLineIndent() {
        final Span span = new Span("   $. > x", 3);
        MatcherAssert.assertThat(
            "a `$.` root head's pos() must also count the line's indent",
            LnReversed.readHead(new Tokens(span.body(), span), span.indent()).pos(),
            Matchers.equalTo(3)
        );
    }

    @Test
    void agreesWithNamedHeadOnIndentedColumn() {
        final Span span = new Span("  foo. > x", 3);
        MatcherAssert.assertThat(
            "a root head and a named head on the same indent must report the same column convention",
            LnReversed.readHead(new Tokens(span.body(), span), span.indent()).pos(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void rejectsAttributeWithoutPrecedingBlankLine() {
        final Emit emit = new Emit();
        new LnReversed(new Span("if. cond then +> t", 2))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `+>` test attribute on a reversed-dispatch line with no blank line above must emit an R-6.5.3 error",
            LnReversedTest.render(emit),
            XhtmlMatchers.hasXPath("/object/errors/error[@line='2']")
        );
    }

    @Test
    void acceptsAttributeAfterBlankLine() {
        final Emit emit = new Emit();
        final Globals globals = new Globals();
        globals.blank();
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        new LnReversed(new Span("  if. cond then +> t", 2))
            .into(stack, globals, emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `+>` test attribute on a reversed-dispatch line preceded by one blank line must not emit any error",
            LnReversedTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/errors"))
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
