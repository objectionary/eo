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
    void emitsHighOctalByteWithoutUnicodeExpansion() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("\\377");
        final Emit emit = new Emit();
        new LnTextBlock(new Span("\"\"\" > bytes", 3))
            .into(new Stack(), globals, emit);
        emit.close();
        MatcherAssert.assertThat(
            "an octal escape in a text block must contribute its raw byte",
            LnTextBlockTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='bytes']/o[@base='Φ.bytes']/o[text()='FF-']"
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
void acceptsOuterBindingAfterCloser() {
    final Globals globals = new Globals();
    globals.openTextBlock(1, 0);
    globals.appendTextLine("hi");
    final Stack stack = new Stack();
    final Emit emit = new Emit();

    new LnTextBlock(new Span("\"\"\":x", 3))
        .into(stack, globals, emit);
    emit.close();

    MatcherAssert.assertThat(
        "a `:name` binding immediately after a text-block closer must be accepted",
        LnTextBlockTest.render(emit),
        XhtmlMatchers.hasXPath(
            "/object/o[@as='x' and @base='Φ.string']"
        )
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

    @Test
    void rejectsInvalidEscapeInTextBlockBody() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("\\uD800");
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnTextBlock(new Span("\"\"\" > x", 3))
                .into(new Stack(), globals, new Emit()),
            "an invalid unicode escape in a text block body must surface as a ParseError, not a raw NumberFormatException"
        );
    }

    @Test
    void rejectsAttributeWithoutPrecedingBlankLine() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 0);
        globals.appendTextLine("hello");
        final Emit emit = new Emit();
        new LnTextBlock(new Span("\"\"\" +> t", 3))
            .into(new Stack(), globals, emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `+>` test attribute on a text-block closer with no blank line above must emit an R-6.5.3 error",
            LnTextBlockTest.render(emit),
            XhtmlMatchers.hasXPath("/object/errors/error[@line='3']")
        );
    }

    @Test
    void acceptsAttributeAfterBlankLine() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 2);
        globals.appendTextLine("hello");
        globals.blank();
        final Emit emit = new Emit();
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        new LnTextBlock(new Span("  \"\"\" +> t", 3))
            .into(stack, globals, emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `+>` test attribute on a text-block closer preceded by one blank line must not emit any error",
            LnTextBlockTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/errors"))
        );
    }

    @Test
    void rejectsBindingOnTextBlockChildUnderFormationParent() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 2);
        globals.appendTextLine("hi");
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.BARE_FORMATION, Openness.OPEN);
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnTextBlock(new Span("  \"\"\":tag", 3))
                .into(stack, globals, new Emit()),
            "a text-block child under a formation parent cannot carry a binding per R-3.12.3"
        );
    }

    @Test
    void acceptsBindingOnTextBlockChildUnderArgumentPositionParent() {
        final Globals globals = new Globals();
        globals.openTextBlock(1, 2);
        globals.appendTextLine("hi");
        final Stack stack = new Stack();
        stack.push(0, 1, Kind.VAPPLICATION, Openness.OPEN);
        Assertions.assertDoesNotThrow(
            () -> new LnTextBlock(new Span("  \"\"\":tag", 3))
                .into(stack, globals, new Emit()),
            "a text-block child in argument position may still carry a binding"
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
