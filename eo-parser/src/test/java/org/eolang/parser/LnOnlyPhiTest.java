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
 * Test case for {@link LnOnlyPhi}.
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
final class LnOnlyPhiTest {

    @Test
    void pushesOnlyPhiKind() {
        final Stack stack = new Stack();
        new LnOnlyPhi(new Span("right > [x] > left", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "an only-phi line must push ONLY_PHI_FORMATION",
            stack.top().kind(),
            Matchers.equalTo(Kind.ONLY_PHI_FORMATION)
        );
    }

    @Test
    void opensBarePhiForVerticalArgs() {
        final Stack stack = new Stack();
        new LnOnlyPhi(new Span("right > [x] > left", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a bare-φ only-phi line must stay OPEN so its φ accepts vertical args",
            stack.top().openness(),
            Matchers.equalTo(Openness.OPEN)
        );
    }

    @Test
    void marksHorizontallyCompletedWhenPhiHasHargs() {
        final Stack stack = new Stack();
        new LnOnlyPhi(new Span("right arg > [x] > left", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "an only-phi whose φ carries horizontal args cannot accept a body — must be HORIZONTAL_COMPLETED",
            stack.top().openness(),
            Matchers.equalTo(Openness.HORIZONTAL_COMPLETED)
        );
    }

    @Test
    void emitsFormationWithName() {
        final Emit emit = new Emit();
        new LnOnlyPhi(new Span("right > [x] > left", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "the only-phi RHS name must attach to the outer <o>",
            LnOnlyPhiTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='left']")
        );
    }

    @Test
    void emitsVoidParamsBeforePhi() {
        final Emit emit = new Emit();
        new LnOnlyPhi(new Span("right > [x y] > pair", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "the only-phi params must emit as void children before the φ slot",
            LnOnlyPhiTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='pair']/o[@name='x' and @base='∅']",
                "/object/o[@name='pair']/o[@name='y' and @base='∅']"
            )
        );
    }

    @Test
    void emitsLhsAsPhiSlot() {
        final Emit emit = new Emit();
        new LnOnlyPhi(new Span("right > [x] > left", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "the LHS expression must emit as a single <o name='φ'> child",
            LnOnlyPhiTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='left']/o[@name='φ' and @base='right']")
        );
    }

    @Test
    void emitsLhsChainFlattenedInsidePhi() {
        final Emit emit = new Emit();
        new LnOnlyPhi(new Span("foo.bar > [x] > left", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an LHS chain must flatten with the last link carrying @name='φ'",
            LnOnlyPhiTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='left']/o[@base='foo' and not(@name)]",
                "/object/o[@name='left']/o[@name='φ' and @base='.bar' and @method='']"
            )
        );
    }

    @Test
    void emitsAutoName() {
        final Emit emit = new Emit();
        new LnOnlyPhi(new Span("right > [x] >>", 5))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `>>` only-phi must emit the cactus auto-name format",
            LnOnlyPhiTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[starts-with(@name,'a🌵')]")
        );
    }

    @Test
    void leavesAnonymousWithoutRightHandName() {
        final Emit emit = new Emit();
        new LnOnlyPhi(new Span("right > [x]", 7))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an only-phi without trailing `> name` leaves the formation anonymous (no @name)",
            LnOnlyPhiTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[not(@name) and not(@base)]"
            )
        );
    }

    @Test
    void acceptsPlusGreaterSuffix() {
        final Emit emit = new Emit();
        new LnOnlyPhi(new Span("right > [x] +> name", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an only-phi may be a `+>` test attribute — the suffix names the formation",
            LnOnlyPhiTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='+name']"
            )
        );
    }

    @Test
    void rejectsEmptyLhs() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnOnlyPhi(new Span("> [x] > foo", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "an only-phi with no LHS must be rejected per R-3.10.6"
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
