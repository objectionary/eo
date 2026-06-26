/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A vertical void-attribute line — R-3.4.7 of the spec.
 *
 * <p>Form: {@code ? > name}. The {@code ?} declares a void attribute on
 * the enclosing formation, equivalent to listing {@code name} among the
 * bracket parameters; it emits the same {@code <o name='name'
 * base='∅'/>} void child (§9.4), which {@code move-voids-up} hoists
 * among the head voids.</p>
 *
 * <p>{@code ? > name} is the only shape the {@code ?} marker may take —
 * never an argument, a method receiver, or anywhere else a value is
 * expected. The marker is therefore <em>not</em> a {@link Value} kind;
 * this line is its sole producer, and a {@code ?} in any value position
 * fails as "expected value".</p>
 *
 * <p>Cross-line behaviour: a closed leaf
 * ({@link Openness#VERTICAL_COMPLETED}), so a deeper-indent line under a
 * void is rejected — a void has no children.</p>
 *
 * @since 0.1
 */
final class LnVoid implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnVoid(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.checkPlain(this.span, globals, emit);
        final Suffix suffix = new Suffix(
            this.span.body().substring(1), this.span, this.span.indent() + 1
        );
        if (suffix.form() != Suffix.Form.NAME || suffix.constant() || suffix.atom()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a void attribute must be written as `? > name`"
            );
        }
        Comments.attach(globals, emit, this.span, suffix.present());
        new Transition(stack, this.span).apply(
            Kind.HEAD, Openness.VERTICAL_COMPLETED, suffix.present()
        );
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            "∅", this.span.line(), this.span.indent()
        );
    }
}
