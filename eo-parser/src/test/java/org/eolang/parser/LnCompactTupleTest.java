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
