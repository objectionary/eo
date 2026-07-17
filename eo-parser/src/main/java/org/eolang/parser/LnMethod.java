/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * A method-dispatch continuation line — §3.5 of the spec.
 *
 * <p>A line starting with {@code .} extends the same-indent
 * predecessor's outer kind. The classifier (Appendix B) produces this
 * shape regardless of how the cross-line machinery (§5.2.3) later
 * resolves it:</p>
 *
 * <ul>
 *   <li>{@link Kind#VMETHOD} when this {@code .method} has 0 horizontal
 *   args — the chain stays open for further {@code .method} continuations
 *   or deeper-indent vapplication children.</li>
 *   <li>{@link Kind#VMETHOD_WITH_HARGS} when this {@code .method}
 *   carries ≥1 horizontal args — the chain becomes
 *   {@link Openness#HORIZONTAL_COMPLETED}.</li>
 * </ul>
 *
 * <p>Rejection paths owned here:</p>
 *
 * <ul>
 *   <li>R-5.2.3(b) — same-indent {@code .method} after a horizontally
 *   completed predecessor.</li>
 *   <li>R-5.2.5 — {@code .method} as a deeper-indent line.</li>
 *   <li>R-5.2.10 — {@code .method} at top level (empty stack).</li>
 * </ul>
 *
 * <p>Emission follows §9.0.3: each chain link is a separate flat
 * sibling {@code <o base='.<name>' method=''>} under the same parent.
 * On entry to this line, the predecessor's open {@code <o>} is closed
 * (cursor exits), then the new link opens — the link's element is the
 * one that remains on the cursor for either more chain continuations
 * or deeper-indent children. The line's optional name suffix attaches
 * to <em>this</em> link's {@code <o>} (the last-link rule means each
 * incoming {@code .method} carries the chain's current "tip" — if the
 * suffix is set, it stays unless a later {@code .method} replaces
 * it).</p>
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.UnnecessaryLocalRule")
final class LnMethod implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnMethod(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.checkPlain(this.span, globals, emit);
        this.precheck(stack);
        final Level top = stack.top();
        final Tokens tokens = this.dottedTokens();
        final boolean fragile = tokens.consumeDispatch();
        final Value method = tokens.readMethodName();
        final List<Value> args = tokens.readArgs();
        Bindings.checkAllOrNothing(args, this.span);
        final String outer = LnApplication.readOuterBinding(tokens);
        final Suffix suffix = new Suffix(
            tokens.tail(), this.span, this.span.indent() + tokens.cursor()
        );
        Comments.seal(globals, emit, this.span);
        if (outer != null && stack.below() != null) {
            stack.below().upgradeArgBinding();
        }
        emit.close();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            ".".concat(method.raw()),
            this.span.line(), method.pos() - 1
        );
        emit.method(fragile);
        if (suffix.constant()) {
            emit.constant();
        }
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
        if (outer != null) {
            emit.slot(Emissions.bindingTag(outer));
        }
        final Kind kind;
        final Openness openness;
        if (args.isEmpty()) {
            kind = Kind.VMETHOD;
            openness = Openness.OPEN;
        } else {
            kind = Kind.VMETHOD_WITH_HARGS;
            openness = Openness.HORIZONTAL_COMPLETED;
        }
        top.become(kind);
        top.close(openness);
        if (suffix.present()) {
            top.name(suffix.label());
        }
        globals.clearBlanks();
        globals.markEmitted();
    }

    /**
     * Validate the line has a predecessor to attach to and that the
     * predecessor's chain is not already horizontally completed —
     * R-5.2.5 / R-5.2.10 / R-5.2.3(b).
     * @param stack Indent stack
     */
    private void precheck(final Stack stack) {
        if (stack.empty() || stack.top().indent() < this.span.indent()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "method continuation has no expression to attach to"
            );
        }
        if (stack.top().openness() == Openness.HORIZONTAL_COMPLETED) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "method continuation not allowed after horizontal application"
            );
        }
        if (stack.top().kind() == Kind.ONLY_PHI_FORMATION) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "method continuation not allowed after only-phi formation"
            );
        }
    }

    /**
     * Build a token stream and verify the line opens a dispatch — a
     * plain {@code .} or the fragile {@code ?.} (R-3.5). The cursor
     * stays on the operator's first character; callers seek past it
     * after recording the column.
     * @return Tokens positioned on the leading dispatch operator
     */
    private Tokens dottedTokens() {
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        if (tokens.atEnd() || !tokens.dispatchAhead()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "method continuation must start with a dot"
            );
        }
        return tokens;
    }
}
