/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

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
 * <p>On execution this line consumes the accumulated body and emits
 * the resulting string literal as a {@code <o base='Φ.string'>}
 * wrapper with a UTF-8 hex {@code <o base='Φ.bytes'>} child carrying
 * the joined body. Per R-3.11.4 a {@code .method} chain after the
 * closing {@code """} is allowed; when present it emits the way
 * {@link ChainEmission} emits it for every other head — flat sibling
 * {@code <o base='.<name>'>} links, with the line's outer binding and
 * name suffix attaching to the last link.</p>
 *
 * @since 0.1
 */
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
        final String body = this.span.body();
        if (!body.startsWith("\"\"\"")) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "text block closer must start with triple-quote"
            );
        }
        final Tokens tokens = new Tokens(body, this.span);
        tokens.seek(3);
        final List<MethodChain> chain = tokens.readChain();
        final String outer = LnApplication.readOuterBinding(tokens, this.span);
        final Suffix suffix = new Suffix(
            tokens.tail(), this.span, this.span.indent() + tokens.cursor()
        );
        suffix.rejectAtomOutsideFormation(this.span);
        if (suffix.test()) {
            Blanks.checkTest(this.span, stack, globals, emit);
        } else {
            Blanks.checkPlain(this.span, globals, emit);
        }
        final byte[] joined = new Unescaped(
            String.join(String.valueOf('\n'), globals.tbody()),
            this.span.line(), this.span.indent()
        ).bytes();
        this.transition(stack, suffix, emit);
        Bindings.observeChild(stack, outer, this.span);
        this.emit(emit, suffix, chain, joined);
        if (!outer.isEmpty()) {
            emit.slot(Emissions.bindingTag(outer));
        }
        globals.closeTextBlock();
        globals.clearBlanks();
        globals.markEmitted();
    }

    private void transition(final Stack stack, final Suffix suffix, final Emit emit) {
        new Transition(stack, this.span, emit).apply(
            Kind.TEXT_BLOCK,
            Openness.VCOMPLETED,
            new Admission(suffix.named(), suffix.test(), suffix.test())
        );
    }

    private void emit(
        final Emit emit, final Suffix suffix, final List<MethodChain> chain,
        final byte[] joined
    ) {
        final String hex = new Hex(joined).asString();
        if (chain.isEmpty()) {
            emit.object(
                suffix.attribute(this.span.line(), this.span.indent()),
                "Φ.string", this.span.line(), this.span.indent()
            );
            new Marked(emit, suffix).apply();
            Emissions.bytesCarrier(emit, this.span.line(), this.span.indent(), hex);
        } else {
            emit.unnamedObject("Φ.string", this.span.line(), this.span.indent());
            Emissions.bytesCarrier(emit, this.span.line(), this.span.indent(), hex);
            emit.close();
            for (int idx = 0; idx < chain.size() - 1; idx = idx + 1) {
                final MethodChain link = chain.get(idx);
                emit.unnamedObject(".".concat(link.name()), this.span.line(), link.dot());
                emit.method(link.fragile());
                emit.close();
            }
            final MethodChain last = chain.get(chain.size() - 1);
            emit.object(
                suffix.attribute(this.span.line(), this.span.indent()),
                ".".concat(last.name()), this.span.line(), last.dot()
            );
            emit.method(last.fragile());
            new Marked(emit, suffix).apply();
        }
    }
}
