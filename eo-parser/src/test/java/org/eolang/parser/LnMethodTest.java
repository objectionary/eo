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
 * Test case for {@link LnMethod}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals", "PMD.UnnecessaryLocalRule"})
final class LnMethodTest {

    @Test
    void rejectsAtTopLevel() {
        final Stack stack = new Stack();
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnMethod(new Span(".bar", 1))
                .into(stack, new Globals(), new Emit()),
            "a `.method` line with an empty stack must be rejected per R-5.2.10"
        );
    }

    @Test
    void rejectsAfterHorizontallyCompletedPredecessor() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo a b > x", 1))
            .into(stack, new Globals(), new Emit());
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnMethod(new Span(".bar", 2))
                .into(stack, new Globals(), new Emit()),
            "a `.method` line after a happlication must be rejected per R-5.2.3(b)"
        );
    }

    @Test
    void extendsHeadToVmethodKind() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo > x", 1))
            .into(stack, new Globals(), new Emit());
        new LnMethod(new Span(".bar > y", 2))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a `.method` continuation must promote the same-indent top to VMETHOD",
            stack.top().kind(),
            Matchers.equalTo(Kind.VMETHOD)
        );
    }

    @Test
    void leavesVmethodOpenForFurtherContinuations() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), new Emit());
        new LnMethod(new Span(".bar", 2))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "vmethod must stay OPEN so further .method continuations can extend the chain",
            stack.top().openness(),
            Matchers.equalTo(Openness.OPEN)
        );
    }

    @Test
    void closesChainOnHargsContinuation() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), new Emit());
        new LnMethod(new Span(".bar 42", 2))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a .method continuation with horizontal args must promote the kind to VMETHOD_WITH_HARGS",
            stack.top().kind(),
            Matchers.equalTo(Kind.VMETHOD_WITH_HARGS)
        );
    }

    @Test
    void marksHargsContinuationHorizontallyCompleted() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), new Emit());
        new LnMethod(new Span(".bar 42", 2))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "VMETHOD_WITH_HARGS must be HORIZONTAL_COMPLETED so no further extension is allowed",
            stack.top().openness(),
            Matchers.equalTo(Openness.HORIZONTAL_COMPLETED)
        );
    }

    @Test
    void emitsFlatSiblingForLink() {
        final Emit emit = new Emit();
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), emit);
        new LnMethod(new Span(".bar > x", 2))
            .into(stack, new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a .method continuation must emit a sibling <o>, not nest inside the predecessor",
            LnMethodTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='foo']",
                "/object/o[@base='.bar' and @method='' and @name='x']"
            )
        );
    }

    @Test
    void recordsDotColumnForChainLink() {
        final Emit emit = new Emit();
        final Stack stack = new Stack();
        new LnFormation(new Span("[] > parent", 1))
            .into(stack, new Globals(), emit);
        new LnApplication(new Span("  foo", 2))
            .into(stack, new Globals(), emit);
        new LnMethod(new Span("  .bar > x", 3))
            .into(stack, new Globals(), emit);
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "the chain link's @pos must equal the column of the leading dot per R-9.1.3",
            LnMethodTest.render(emit),
            XhtmlMatchers.hasXPath("//o[@base='.bar' and @pos='2']")
        );
    }

    @Test
    void marksLevelNamedWhenContinuationCarriesSuffix() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), new Emit());
        new LnMethod(new Span(".bar > x", 2))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a continuation carrying `> name` must flip the level's named flag",
            stack.top().named(),
            Matchers.is(true)
        );
    }

    @Test
    void emitsHargsAsChildrenOfLastLink() {
        final Emit emit = new Emit();
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), emit);
        new LnMethod(new Span(".bar 42 > x", 2))
            .into(stack, new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "horizontal args on a .method line must nest inside the link's <o>",
            LnMethodTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='.bar']/o[@base='Φ.number']")
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
