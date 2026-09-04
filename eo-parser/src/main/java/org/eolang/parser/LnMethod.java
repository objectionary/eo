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
 * <li>{@link Kind#VMETHOD} when this {@code .method} has 0 horizontal
 * args — the chain stays open for further {@code .method} continuations
 * or deeper-indent vapplication children.</li>
 * <li>{@link Kind#VMETHOD_HARGS} when this {@code .method}
 * carries one or more horizontal args — the chain becomes
 * {@link Openness#HCOMPLETED}.</li>
 * </ul>
 *
 * <p>Rejection paths owned here:</p>
 *
 * <ul>
 * <li>R-5.2.3(b) — same-indent {@code .method} after a horizontally
 * completed predecessor.</li>
 * <li>R-3.8.3 — {@code .method} as the receiver of a bare reversed
 * dispatch, which may not begin with a dot.</li>
 * <li>R-5.2.5 — {@code .method} as a deeper-indent line.</li>
 * <li>R-5.2.10 — {@code .method} at top level (empty stack).</li>
 * <li>R-6.6.4 — a {@code .method} continuation after a link that
 * carries an inline binding, which the continuation would leave on a
 * link the chain no longer ends with.</li>
 * <li>R-6.3.3 — a {@code .method} continuation on a predecessor whose
 * naming line declared it a test attribute, which would otherwise
 * overwrite that attribute's label with the chain's own.</li>
 * <li>R-6.3.1 / R-5.3.4 — a {@code .method} continuation re-purposing
 * the top entry when its parent is an atom, unless the continuation
 * itself carries a test-attribute suffix. {@link Transition#apply}
 * enforces the same rule for every other line shape via
 * {@link Level#patom()}; this line never reaches {@link Transition}
 * (it seals the top entry instead of pushing or replacing it), so the
 * check is repeated here directly on {@link Level#patom()}.</li>
 * </ul>
 *
 * <p>Emission follows §9.0.3: each chain link is a separate flat
 * sibling {@code <o base='.<name>' method=''>} under the same parent.
 * On entry to this line, the predecessor's open {@code <o>} is closed
 * (cursor exits), then the new link opens — the link's element is the
 * one that remains on the cursor for either more chain continuations
 * or deeper-indent children. The line's optional name suffix attaches
 * to <em>this</em> link's {@code <o>} only: per R-6.2.3 an
 * intermediate name is independent of the chain's outermost name, so
 * closing the predecessor's link (§5.2.5) also forgets any name it
 * carried — only the last link's own suffix, if any, names the
 * chain.</p>
 *
 * @since 0.1
 */
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
        this.precheck(stack);
        final Level top = stack.top();
        final Tokens tokens = this.dottedTokens();
        final boolean fragile = tokens.consumeDispatch();
        final Value method = tokens.readMethodName();
        final List<Value> args = tokens.readArgs();
        Bindings.checkAllOrNothing(args, this.span);
        final String outer = LnApplication.readOuterBinding(tokens, this.span);
        final Suffix suffix = new Suffix(
            tokens.tail(), this.span, this.span.indent() + tokens.cursor()
        );
        suffix.rejectAtomOutsideFormation(this.span);
        this.checkAtom(top, suffix);
        if (suffix.test()) {
            Blanks.checkTest(this.span, stack, globals, emit);
        } else {
            Blanks.checkPlain(this.span, globals, emit);
        }
        globals.seal(emit, this.span);
        if (!outer.isEmpty()) {
            final Level under = stack.below();
            Bindings.checkReceiverUpgrade(under, this.span);
            under.upgradeArgBinding();
        }
        stack.seal();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            ".".concat(method.raw()),
            this.span.line(), method.pos() - 1
        );
        emit.method(fragile);
        new Marked(emit, suffix).apply();
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
        if (!outer.isEmpty()) {
            emit.slot(Emissions.bindingTag(outer));
        }
        final Kind kind;
        final Openness openness;
        if (args.isEmpty()) {
            kind = Kind.VMETHOD;
            openness = Openness.OPEN;
        } else {
            kind = Kind.VMETHOD_HARGS;
            openness = Openness.HCOMPLETED;
        }
        top.become(kind);
        top.close(openness);
        if (!outer.isEmpty()) {
            top.tie();
        }
        if (suffix.present()) {
            top.name(suffix.label(), suffix.test());
        }
        globals.clearBlanks();
        globals.markEmitted();
    }

    private void precheck(final Stack stack) {
        if (!stack.empty() && stack.top().kind() == Kind.BARE_REVERSED
            && !stack.top().taken()
            && stack.top().indent() < this.span.indent()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "reversed dispatch receiver must not begin with dot"
            );
        }
        if (stack.empty() || stack.top().indent() < this.span.indent()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "method continuation has no expression to attach to"
            );
        }
        if (stack.top().openness() == Openness.HCOMPLETED) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "method continuation not allowed after horizontal application, try vertical application instead"
            );
        }
        if (stack.top().kind() == Kind.ONLY_PHI) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "method continuation not allowed after only-phi formation"
            );
        }
        final String refusal = stack.top().refusal();
        if (!refusal.isEmpty()) {
            throw new ParseError(this.span.line(), this.span.indent(), refusal);
        }
    }

    private void checkAtom(final Level top, final Suffix suffix) {
        if (top.patom() && !suffix.test()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "atom may contain only test attributes"
            );
        }
    }

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
