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
 * Test case for {@link LnCompactTuple}.
 * @since 0.1
 */
final class LnCompactTupleTest {

    @Test
    void pushesCompactTupleKind() {
        final Stack stack = new Stack();
        new LnCompactTuple(new Span("sprintf *1 > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a `head *N` line must push COMPACT_TUPLE",
            stack.top().kind(),
            Matchers.equalTo(Kind.COMPACT_TUPLE)
        );
    }

    @Test
    void recordsNFromExplicitInteger() {
        final Stack stack = new Stack();
        new LnCompactTuple(new Span("sprintf *2 > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "the parsed *N value must round-trip on the pushed level for the close-time check",
            stack.top().count(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void defaultsNToZeroWhenAbsent() {
        final Stack stack = new Stack();
        new LnCompactTuple(new Span("sprintf * > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "an absent N must default to 0 per R-3.9.1",
            stack.top().count(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void recordsNFromMultiDigitInteger() {
        final Stack stack = new Stack();
        new LnCompactTuple(new Span("sprintf *123 > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a multi-digit *N value must accumulate across every digit read",
            stack.top().count(),
            Matchers.equalTo(123)
        );
    }

    @Test
    void recordsZeroFromExplicitZeroDigit() {
        final Stack stack = new Stack();
        new LnCompactTuple(new Span("sprintf *0 > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "an explicit *0 must record the same count as an absent N",
            stack.top().count(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void rejectsCountWithLeadingZeros() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnCompactTuple(new Span("sprintf *007 > x", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "a count spelled with leading zeros is no more an integer than 007 is"
        );
    }

    @Test
    void rejectsCountAboveIntegerMax() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnCompactTuple(new Span("sprintf *99999999999 > x", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "a *N value past Integer.MAX_VALUE must raise a positioned ParseError"
        );
    }

    @Test
    void rejectsCountAboveIntegerMaxWithCanonicalMessage() {
        MatcherAssert.assertThat(
            "the overflow error must carry the canonical §9.9 message",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnCompactTuple(new Span("sprintf *99999999999 > x", 1))
                    .into(new Stack(), new Globals(), new Emit())
            ).getMessage(),
            Matchers.equalTo("compact tuple count is too large")
        );
    }

    @Test
    void leavesLevelOpenForVerticalChildren() {
        final Stack stack = new Stack();
        new LnCompactTuple(new Span("sprintf *1 > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "compact-tuple must remain OPEN to receive its deeper-indent children",
            stack.top().openness(),
            Matchers.equalTo(Openness.OPEN)
        );
    }

    @Test
    void emitsHeadElement() {
        final Emit emit = new Emit();
        new LnCompactTuple(new Span("sprintf *1 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "compact-tuple must emit the head's <o> with the user-supplied name",
            LnCompactTupleTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='sprintf']")
        );
    }

    @Test
    void rejectsAttributeWithoutPrecedingBlankLine() {
        final Emit emit = new Emit();
        new LnCompactTuple(new Span("sprintf *1 +> t", 2))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `+>` test attribute on a compact-tuple line with no blank line above must emit an R-6.5.3 error",
            LnCompactTupleTest.render(emit),
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
        new LnCompactTuple(new Span("  sprintf *1 +> t", 2))
            .into(stack, globals, emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `+>` test attribute on a compact-tuple line preceded by one blank line must not emit any error",
            LnCompactTupleTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/errors"))
        );
    }

    @Test
    void acceptsIdentityHeadWithoutChain() {
        final Stack stack = new Stack();
        new LnCompactTuple(new Span("I *2 > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "an I head is not chainable, so the empty-chain branch must still push COMPACT_TUPLE",
            stack.top().kind(),
            Matchers.equalTo(Kind.COMPACT_TUPLE)
        );
    }

    @Test
    void emitsIdentityHeadAsVoidFormation() {
        final Emit emit = new Emit();
        new LnCompactTuple(new Span("I *2 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an I head must open a baseless formation holding a void and a phi bound to it",
            LnCompactTupleTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and not(@base)]/o[@name='φ' and @base='x']")
        );
    }

    @Test
    void emitsTerminatorHeadAsErrorBase() {
        final Emit emit = new Emit();
        new LnCompactTuple(new Span("T *1 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a compact tuple headed by the bare T term must emit the error base",
            LnCompactTupleTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='⊥']")
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
