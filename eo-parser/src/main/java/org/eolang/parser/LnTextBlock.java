/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A triple-quoted text-block closer line — §3.11 of the spec.
 *
 * <p>A {@code """} on its own line (at the opener's indent) terminates
 * a text block opened earlier. This line is dispatched <em>only</em>
 * after {@link Eo} has accumulated the body lines into
 * {@link Globals#tbody()} via in-flight state tracking — the opener
 * line and any body lines are handled directly by {@link Eo#process}
 * (their pre-classification special path).</p>
 *
 * <p>On execution this line consumes the accumulated body, pushes a
 * {@link Kind#TEXT_BLOCK} level at the opener's indent, and emits the
 * resulting string literal as a {@code <o base='Φ.string'>} wrapper
 * with a UTF-8 hex {@code <o base='Φ.bytes'>} child carrying the joined
 * body. Method chains and arguments after the closing {@code """} are
 * deferred to a later iteration. *
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.UnnecessaryLocalRule")
final class LnTextBlock implements Line {

    /**
     * The line's source span (the closing {@code """} line).
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnTextBlock(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.checkPlain(this.span, globals, emit);
        final String body = this.span.body();
        if (!body.startsWith("\"\"\"")) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "text block closer must start with triple-quote"
            );
        }
        final Tokens tokens = new Tokens(body, this.span);
        tokens.seek(3);
        tokens.readChain();
        final Suffix suffix = new Suffix(
            tokens.tail(), this.span, this.span.indent() + tokens.cursor()
        );
        final String joined = Emissions.unescapeBody(
            String.join(String.valueOf('\n'), globals.tbody()).trim()
        );
        this.transition(stack, suffix);
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            "Φ.string",
            this.span.line(), this.span.indent()
        );
        if (suffix.constant()) {
            emit.constant();
        }
        Emissions.bytesCarrier(
            emit, this.span.line(), this.span.indent(),
            new Hex(joined).asString()
        );
        globals.closeTextBlock();
        globals.clearBlanks();
        globals.markEmitted();
    }

    /**
     * Push or replace the stack level at the closer's indent.
     * @param stack The stack
     * @param suffix The parsed suffix
     */
    private void transition(final Stack stack, final Suffix suffix) {
        new Transition(stack, this.span).apply(
            Kind.TEXT_BLOCK, Openness.VERTICAL_COMPLETED, suffix.named()
        );
    }
}
