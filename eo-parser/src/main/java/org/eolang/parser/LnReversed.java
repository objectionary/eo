/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * A reversed-dispatch line — §3.8 of the spec.
 *
 * <p>Form: {@code name. [args] [> name | >> | +> name]}. The trailing
 * dot transforms a NAME identifier into the prefix-notation form of
 * method dispatch: {@code if. cond then else} means "dispatch
 * {@code .if} on receiver {@code cond} with method args
 * {@code then else}".</p>
 *
 * <p>Two flavours by argument count:</p>
 *
 * <ul>
 * <li><strong>Horizontal</strong> ({@code name. arg1 arg2}) —
 * {@code arg1} is the receiver, {@code arg2…} are method args. Outer
 * kind {@link Kind#REVERSED_HARGS},
 * {@link Openness#HCOMPLETED}. No deeper-indent children.</li>
 * <li><strong>Vertical</strong> ({@code name.} with no hargs) — the
 * next deeper-indent line is the receiver (R-5.2.9), subsequent
 * deeper-indent siblings are method args. Outer kind
 * {@link Kind#BARE_REVERSED}, {@link Openness#OPEN}. R-5.3.2 fires
 * at close time if no receiver appeared.</li>
 * </ul>
 *
 * <p>Emission: opens {@code <o base='.<name>' method=''>} at the
 * current cursor and stays inside. For horizontal form, the receiver
 * and method args are appended as children before the cursor closes.
 * For vertical form, deeper-indent lines (dispatched through
 * {@link LnApplication} etc.) attach as children automatically.</p>
 *
 * <p>R-3.8.1 restricts the head identifier to a single {@code NAME},
 * {@code @}, {@code ^}, or {@code $} token — no dotted paths and no
 * {@code ROOT}/literal-rooted prefixes.</p>
 *
 * @since 0.1
 */
final class LnReversed implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnReversed(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        final Value head = LnReversed.readHead(tokens, this.span.indent());
        if (tokens.atEnd() || !tokens.dispatchAhead()) {
            throw new ParseError(
                this.span.line(), this.span.indent() + tokens.cursor(),
                "reversed dispatch must end with a dot"
            );
        }
        final boolean fragile = tokens.consumeDispatch();
        final List<Value> args = tokens.readArgs();
        if (!args.isEmpty()) {
            Bindings.checkReceiver(args.get(0), this.span);
            Bindings.checkAllOrNothing(
                args.subList(1, args.size()), this.span
            );
        }
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
        globals.seal(emit, this.span);
        final Kind kind;
        final Openness openness;
        if (args.isEmpty()) {
            kind = Kind.BARE_REVERSED;
            openness = Openness.OPEN;
        } else {
            kind = Kind.REVERSED_HARGS;
            openness = Openness.HCOMPLETED;
        }
        this.transition(stack, suffix, kind, openness, emit);
        Bindings.observeChild(stack, outer, this.span);
        globals.clearBlanks();
        globals.markEmitted();
        this.emit(emit, suffix, ".".concat(head.raw()), fragile, args, outer);
    }

    static Value readHead(final Tokens tokens, final int indent) {
        final Value value;
        if (!tokens.atEnd() && LnReversed.rootHead(tokens.current())) {
            final int start = tokens.cursor();
            final String mapped = LnReversed.rootSymbol(tokens.current());
            tokens.seek(start + 1);
            value = new Value(Value.Kind.IDENTIFIER, mapped, indent + start);
        } else {
            value = tokens.readName();
        }
        return value;
    }

    static String rootSymbol(final char glyph) {
        final String mapped;
        if (glyph == '@') {
            mapped = "φ";
        } else if (glyph == '^') {
            mapped = "ρ";
        } else {
            mapped = "ξ";
        }
        return mapped;
    }

    private void emit(
        final Emit emit, final Suffix suffix, final String base,
        final boolean fragile, final List<Value> args, final String outer
    ) {
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            base, this.span.line(), this.span.indent()
        );
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
        }
        if (fragile) {
            emit.fragile();
        }
        if (suffix.constant()) {
            emit.constant();
        }
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
        if (!outer.isEmpty()) {
            emit.slot(Emissions.bindingTag(outer));
        }
    }

    private void transition(
        final Stack stack, final Suffix suffix, final Kind kind, final Openness openness,
        final Emit emit
    ) {
        new Transition(stack, this.span, emit).apply(
            kind, openness, new Admission(suffix.named(), suffix.test(), suffix.test())
        );
    }

    private static boolean rootHead(final char glyph) {
        return glyph == '@' || glyph == '^' || glyph == '$';
    }
}
