/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * An only-phi formation line — §3.10 / §4.5 of the spec.
 *
 * <p>Form: {@code lhs > [params] > name}. The {@code lhs} is a
 * horizontal expression that becomes the {@code φ} slot of an
 * anonymous formation; the formation has {@code params} as voids and
 * is named by the right-hand suffix.</p>
 *
 * <p>Mechanics (R-3.10.1):</p>
 *
 * <ul>
 *   <li>LHS is parsed as an application expression (head + optional
 *   chain + optional hargs). Its outermost {@code <o>} carries
 *   {@code @name='φ'} per the emission shape.</li>
 *   <li>Params inside the brackets become void children of the
 *   formation, emitted before the φ slot.</li>
 *   <li>The right-hand suffix names the formation (or auto-names with
 *   {@code >>}).</li>
 * </ul>
 *
 * <p>Outer kind: {@link Kind#ONLY_PHI_FORMATION}. Openness depends on
 * the φ (the LHS): with zero horizontal args the φ is
 * {@link Openness#OPEN}, so deeper-indent lines attach to it as
 * vertical application arguments (§4.5) — {@code foo > [x] > bar} with
 * a body block is {@code [x] > bar} whose φ is {@code foo} applied to
 * that block. With horizontal args the φ is already a full application
 * and the line is {@link Openness#HORIZONTAL_COMPLETED} — no body is
 * accepted. An only-phi argument may not carry a name suffix (the
 * formation binds only φ); the {@link Stack} flags such arguments and
 * the close-time check in {@link Eo} rejects a name on them.</p>
 *
 * <p>This iteration accepts identifier and root LHS heads with
 * optional chains and identifier / INT / STAR / STRING / FLOAT /
 * ROOT horizontal args. R-3.10.6 LHS restrictions are honoured by
 * scanner exclusion (formations and reversed-with-hargs LHS are not
 * accepted as inputs because their classifiers fire first). *
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.UnnecessaryLocalRule")
final class LnOnlyPhi implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnOnlyPhi(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.enterAfterMeta(this.span, globals, emit);
        final String body = this.span.body();
        final int phi = Eo.topLevelGreaterBracketIndex(body);
        if (phi < 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "only-phi formation must contain `> [`"
            );
        }
        final int bracket = phi + 2;
        final int close = body.indexOf(']', bracket);
        if (close < 0) {
            throw new ParseError(
                this.span.line(), this.span.indent() + bracket,
                "only-phi parameter list missing closing `]`"
            );
        }
        final String lhs = body.substring(0, phi).stripTrailing();
        if (lhs.isEmpty()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "only-phi formation requires a non-empty body before `> [`"
            );
        }
        final List<String> params = LnOnlyPhi.parseParams(
            body.substring(bracket + 1, close), this.span, bracket + 1
        );
        final Suffix suffix = new Suffix(
            body.substring(close + 1), this.span, this.span.indent() + close + 1
        );
        final Span inner = new Span(
            " ".repeat(this.span.indent()).concat(lhs), this.span.line()
        );
        final Tokens tokens = new Tokens(inner.body(), inner);
        final Value head = tokens.readValue();
        final List<MethodChain> chain = tokens.readChain();
        final List<Value> args = tokens.readArgs();
        final boolean open = args.isEmpty();
        Comments.attach(globals, emit, this.span, suffix.present() || suffix.test());
        this.transition(stack, suffix, open);
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            null, this.span.line(), this.span.indent()
        );
        if (suffix.constant()) {
            emit.constant();
        }
        int column = this.span.indent() + bracket + 1;
        for (final String param : params) {
            final String mapped;
            if (param.equals("@")) {
                mapped = "φ";
            } else {
                mapped = param;
            }
            emit.voidParam(mapped, this.span.line(), column);
            column = column + param.length() + 1;
        }
        this.emitLhs(emit, head, chain, args, open);
    }

    /**
     * Emit the pre-parsed LHS expression as a single child with
     * {@code @name='φ'}. Emission mirrors {@link LnApplication}: the
     * topmost element (head with no chain, or the chain's last link)
     * carries {@code @name='φ'}. When {@code open} the φ {@code <o>} is
     * left on the cursor so deeper-indent lines attach to it as
     * vertical arguments, and the {@link Stack.Closer} closes both it
     * and the formation; otherwise the φ is closed here (its horizontal
     * args are complete) and only the formation stays open.
     * @param emit Emitter
     * @param head The pre-read head value
     * @param chain The pre-read method chain (may be empty)
     * @param args The pre-read horizontal args (may be empty)
     * @param open Whether the φ stays open for vertical arguments
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    private void emitLhs(
        final Emit emit, final Value head, final List<MethodChain> chain,
        final List<Value> args, final boolean open
    ) {
        if (chain.isEmpty()) {
            Emissions.openValue(emit, "φ", head, this.span.line());
        } else {
            Emissions.openValue(emit, null, head, this.span.line());
            emit.close();
            for (int idx = 0; idx < chain.size() - 1; idx = idx + 1) {
                final MethodChain link = chain.get(idx);
                emit.object(null, ".".concat(link.name()), this.span.line(), link.dot());
                emit.method(link.fragile());
                emit.close();
            }
            final MethodChain last = chain.get(chain.size() - 1);
            emit.object("φ", ".".concat(last.name()), this.span.line(), last.dot());
            emit.method(last.fragile());
        }
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
        if (!open) {
            emit.close();
        }
    }

    /**
     * Push or replace the stack level per Step B/C/D of §5.2. A bare
     * (zero-hargs) φ opens the level for vertical arguments; a φ that
     * already carries horizontal args is horizontally completed.
     * @param stack The stack
     * @param suffix Right-hand-side suffix
     * @param open Whether the φ has no horizontal args
     */
    private void transition(final Stack stack, final Suffix suffix, final boolean open) {
        final Openness openness;
        if (open) {
            openness = Openness.OPEN;
        } else {
            openness = Openness.HORIZONTAL_COMPLETED;
        }
        new Transition(stack, this.span).apply(
            Kind.ONLY_PHI_FORMATION, openness, suffix.present() || suffix.test()
        );
    }

    /**
     * Parse the only-phi parameter list.
     * @param text The text between brackets
     * @param span Source span (for error)
     * @param origin Column of the first char after {@code [}
     * @return Parameter names
     */
    private static List<String> parseParams(
        final String text, final Span span, final int origin
    ) {
        final List<String> out = new ArrayList<>(0);
        if (!text.isEmpty()
            && (text.charAt(0) == ' ' || text.charAt(text.length() - 1) == ' ')) {
            throw new ParseError(
                span.line(), span.indent() + origin,
                "formation brackets must not contain leading or trailing space"
            );
        }
        int idx = 0;
        while (idx < text.length()) {
            int end = idx;
            while (end < text.length() && text.charAt(end) != ' ') {
                end = end + 1;
            }
            out.add(text.substring(idx, end));
            if (end < text.length()) {
                if (end + 1 < text.length() && text.charAt(end + 1) == ' ') {
                    throw new ParseError(
                        span.line(), span.indent() + origin + end,
                        "parameter names in voids must be separated by exactly one space"
                    );
                }
                idx = end + 1;
            } else {
                idx = end;
            }
        }
        return out;
    }
}
