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
 * Test case for {@link LnTextBlock}.
 * @since 0.1
 */
final class LnTextBlockTest {

    @Test
    void pushesTextBlockKindAtCloser() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("hello");
        final Stack stack = new Stack();
        new LnTextBlock(new Span("\"\"\" > x", 3))
            .into(stack, globals, new Emit());
        MatcherAssert.assertThat(
            "the closing line must push a TEXT_BLOCK level for the consolidated literal",
            stack.top().kind(),
            Matchers.equalTo(Kind.TEXT_BLOCK)
        );
    }

    @Test
    void emitsStringWithBytes() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("hello");
        final Emit emit = new Emit();
        new LnTextBlock(new Span("\"\"\" > greeting", 3))
            .into(new Stack(), globals, emit);
        emit.close();
        MatcherAssert.assertThat(
            "a text-block closer must emit Φ.string with UTF-8 bytes carrying the body",
            LnTextBlockTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='greeting' and @base='Φ.string']",
                "/object/o[@name='greeting']/o[@base='Φ.bytes']/o[text()='68-65-6C-6C-6F']"
            )
        );
    }

    @Test
    void joinsMultipleBodyLinesWithNewline() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("hello");
        globals.appendTextLine("world");
        final Emit emit = new Emit();
        new LnTextBlock(new Span("\"\"\" > greeting", 4))
            .into(new Stack(), globals, emit);
        emit.close();
        MatcherAssert.assertThat(
            "multi-line bodies must be joined by `\\n` before UTF-8 encoding (0x0A between)",
            LnTextBlockTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='greeting']/o[@base='Φ.bytes']/o[text()='68-65-6C-6C-6F-0A-77-6F-72-6C-64']"
            )
        );
    }

    @Test
    void resetsTextBlockStateAfterEmission() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("hi");
        new LnTextBlock(new Span("\"\"\" > x", 3))
            .into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "after emitting, the in-text-block flag must clear so subsequent lines parse normally",
            globals.inTextBlock(),
            Matchers.is(false)
        );
    }

    @Test
    void acceptsChainAfterCloserWithoutSuffix() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("hi");
        final Stack stack = new Stack();
        new LnTextBlock(new Span("\"\"\".as-bytes", 3))
            .into(stack, globals, new Emit());
        MatcherAssert.assertThat(
            "a `.method` chain right after the closer (deferred, not yet emitted) must not"
                .concat(" be rejected as trailing garbage"),
            stack.top().kind(),
            Matchers.equalTo(Kind.TEXT_BLOCK)
        );
    }

    @Test
    void marksLevelNamedWhenSuffixFollowsChain() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("hi");
        final Stack stack = new Stack();
        new LnTextBlock(new Span("\"\"\".as-bytes > greeting", 3))
            .into(stack, globals, new Emit());
        MatcherAssert.assertThat(
            "a `> name` suffix following a deferred chain must still be parsed",
            stack.top().named(),
            Matchers.is(true)
        );
    }

    @Test
    void marksLevelNamedWhenSuffixPresent() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        final Stack stack = new Stack();
        new LnTextBlock(new Span("\"\"\" > greeting", 2))
            .into(stack, globals, new Emit());
        MatcherAssert.assertThat(
            "a closer carrying `> name` must mark the level as named",
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
