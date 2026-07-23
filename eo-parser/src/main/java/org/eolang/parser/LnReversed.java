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
 *   <li><strong>Horizontal</strong> ({@code name. arg1 arg2}) —
 *   {@code arg1} is the receiver, {@code arg2…} are method args. Outer
 *   kind {@link Kind#REVERSED_WITH_HARGS},
 *   {@link Openness#HORIZONTAL_COMPLETED}. No deeper-indent
 *   children.</li>
 *   <li><strong>Vertical</strong> ({@code name.} with no hargs) — the
 *   next deeper-indent line is the receiver (R-5.2.9), subsequent
 *   deeper-indent siblings are method args. Outer kind
 *   {@link Kind#BARE_REVERSED}, {@link Openness#OPEN}. R-5.3.2 fires
 *   at close time if no receiver appeared.</li>
 * </ul>
 *
 * <p>Emission: opens {@code <o base='.<name>' method=''>} at the
 * current cursor and stays inside. For horizontal form, the receiver
 * and method args are appended as children before the cursor closes.
 * For vertical form, deeper-indent lines (dispatched through
 * {@link LnApplication} etc.) attach as children automatically.</p>
 *
 * <p>R-3.8.1 restricts the receiver-identifier to a single {@code NAME}
 * token (or {@code @} / {@code ^}). This iteration accepts {@code NAME}
 * only; {@code @} and {@code ^} attach in a later round. *
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.UnnecessaryLocalRule")
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
        Blanks.checkPlain(this.span, globals, emit);
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        final Value head = LnReversed.readHead(tokens);
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
        final String outer = LnApplication.readOuterBinding(tokens);
        final Suffix suffix = new Suffix(
            tokens.tail(), this.span, this.span.indent() + tokens.cursor()
        );
        Comments.seal(globals, emit, this.span);
        final Kind kind;
        final Openness openness;
        if (args.isEmpty()) {
            kind = Kind.BARE_REVERSED;
            openness = Openness.OPEN;
        } else {
            kind = Kind.REVERSED_WITH_HARGS;
            openness = Openness.HORIZONTAL_COMPLETED;
        }
        this.transition(stack, suffix, kind, openness);
        Bindings.observeChild(stack, outer, this.span);
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            ".".concat(head.raw()),
            this.span.line(), this.span.indent()
        );
        if (fragile) {
            emit.fragile();
        }
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
        }
        if (suffix.constant()) {
            emit.constant();
        }
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
        if (outer != null) {
            emit.slot(Emissions.bindingTag(outer));
        }
    }

    /**
     * Push or replace the stack level per Step B/C/D of §5.2.
     * @param stack The stack
     * @param suffix The parsed suffix
     * @param kind Initial outer kind
     * @param openness Initial openness
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private void transition(
        final Stack stack, final Suffix suffix, final Kind kind, final Openness openness
    ) {
        new Transition(stack, this.span).apply(kind, openness, suffix.named());
    }

    /**
     * Read the reversed-dispatch head — either a {@code NAME}
     * identifier or one of the root tokens {@code @} / {@code ^} /
     * {@code $} (mapped to {@code φ} / {@code ρ} / {@code ξ} per
     * R-9.3). The cursor is left at the trailing dot.
     * @param tokens Token reader positioned at the head
     * @return Head value with the XMIR symbol already mapped
     */
    private static Value readHead(final Tokens tokens) {
        final Value value;
        if (!tokens.atEnd() && LnReversed.rootHead(tokens.current())) {
            final int start = tokens.cursor();
            final String mapped = LnReversed.rootSymbol(tokens.current());
            tokens.seek(start + 1);
            value = new Value(Value.Kind.IDENTIFIER, mapped, start, start + 1);
        } else {
            value = tokens.readName();
        }
        return value;
    }

    /**
     * Whether the character is one of the reversed-dispatch root
     * head tokens ({@code @}, {@code ^}, {@code $}).
     * @param glyph Source character
     * @return True for a root head
     */
    private static boolean rootHead(final char glyph) {
        return glyph == '@' || glyph == '^' || glyph == '$';
    }

    /**
     * Map a root head character to its XMIR symbol per R-9.3.
     * @param glyph One of {@code @}, {@code ^}, {@code $}
     * @return XMIR symbol
     */
    private static String rootSymbol(final char glyph) {
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
}
