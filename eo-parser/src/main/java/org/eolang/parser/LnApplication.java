/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * An application line — §3.6 of the spec.
 *
 * <p>Form: {@code head [arg…] [> name]}. The head is one of: a paren
 * group, a data literal, a {@code *} star tuple, or a bare identifier
 * with optional chained {@code .method.method} segments. After the
 * head, zero or more space-separated arguments may follow.</p>
 *
 * <p>Outer kinds produced (Appendix A):</p>
 *
 * <ul>
 * <li>{@link Kind#HEAD} — head only, no chain, no args. Open for
 * deeper-indent children (promotes to {@code VAPPLICATION}).</li>
 * <li>{@link Kind#HMETHOD} — head with {@code .method} chain, 0
 * horizontal args. Open for deeper-indent children.</li>
 * <li>{@link Kind#HAPPLICATION} — head (with or without chain) plus one
 * or more horizontal args. {@link Openness#HCOMPLETED}.</li>
 * </ul>
 *
 * <p>Emission follows §9.0.3: method-dispatch chains emit as
 * <em>flat siblings</em> under the enclosing parent — the receiver as
 * the first sibling, each {@code .method} link as a separate
 * {@code <o base='.<name>' method=''>} sibling. The chain's
 * outermost user-given name attaches to the <em>last</em> link
 * (R-9.0.3.1). Horizontal args become children of the head's {@code <o>}
 * — or of the chain's last link when the head is chained.</p>
 *
 * <p>This iteration handles identifier and star heads with optional
 * dotted chains and identifier / INT horizontal args. Paren groups,
 * string / float / hex / bytes literals, and inline bindings attach in
 * subsequent iterations.</p>
 *
 * @since 0.1
 */
final class LnApplication implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnApplication(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        final Value head = tokens.readValue();
        final List<MethodChain> chain = tokens.readChain();
        final List<Value> args = tokens.readArgs();
        Bindings.checkAllOrNothing(args, this.span);
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
        this.checkGroupHead(head, chain, args, outer);
        globals.seal(emit, this.span);
        final Kind kind = LnApplication.classify(head, chain, args);
        final Openness openness;
        if (kind == Kind.HAPPLICATION) {
            openness = Openness.HCOMPLETED;
        } else {
            openness = Openness.OPEN;
        }
        this.transition(stack, suffix, kind, openness);
        Bindings.observeChild(stack, outer, this.span);
        globals.clearBlanks();
        globals.markEmitted();
        this.emit(emit, suffix, head, chain, args);
        if (!outer.isEmpty()) {
            emit.slot(Emissions.bindingTag(outer));
        }
    }

    /**
     * Read an optional outer {@code :binding} that follows the line's
     * value/chain/args before the suffix — per §3.12 the binding may
     * attach to the line's whole expression when it occupies an
     * argument position (a deeper-indent child of a vapplication or
     * vertical reversed dispatch).
     *
     * <p>A chain that goes on after the binding is rejected here per
     * R-6.6.4 — the binding would sit on a method the chain does not end
     * with.</p>
     *
     * <p>A line that carries no binding gets the empty string, not
     * {@code null}: §3.12 spells no empty label and
     * {@link Tokens#readBinding()} rejects one, so the empty string
     * names absence and nothing else (#8029).</p>
     *
     * @param tokens Token reader
     * @param span Source span of the line
     * @return The binding label, empty when the line carries none
     */
    static String readOuterBinding(final Tokens tokens, final Span span) {
        final String label;
        if (!tokens.atEnd() && tokens.current() == ':') {
            final int start = tokens.cursor();
            tokens.seek(start + 1);
            label = tokens.readBinding();
            if (!tokens.atEnd() && tokens.current() == '.') {
                throw new ParseError(
                    span.line(), span.indent() + start,
                    "inline binding allowed only on the last method in a chain"
                );
            }
        } else {
            label = "";
        }
        return label;
    }

    private static Kind classify(
        final Value head, final List<MethodChain> chain, final List<Value> args
    ) {
        final Kind kind;
        if (args.isEmpty()) {
            if (chain.isEmpty()) {
                kind = LnApplication.bare(head);
            } else {
                kind = Kind.HMETHOD;
            }
        } else {
            kind = Kind.HAPPLICATION;
        }
        return kind;
    }

    private static Kind bare(final Value head) {
        final Kind kind;
        if (head.identity()) {
            kind = Kind.IDENTITY_OBJECT;
        } else {
            kind = Kind.HEAD;
        }
        return kind;
    }

    private void checkGroupHead(
        final Value head, final List<MethodChain> chain, final List<Value> args,
        final String outer
    ) {
        if (head.group()
            && chain.isEmpty() && args.isEmpty() && outer.isEmpty()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "redundant parentheses around a top-level expression — drop the outer `(` and `)`"
            );
        }
        if (!args.isEmpty() && head.opensFormationBody()) {
            final String reason;
            if (head.identity()) {
                reason = Emissions.NO_IDENTITY_ARGS;
            } else {
                reason = "horizontal formation not allowed as argument";
            }
            throw new ParseError(this.span.line(), head.pos(), reason);
        }
    }

    private void transition(
        final Stack stack, final Suffix suffix, final Kind kind, final Openness openness
    ) {
        new Transition(stack, this.span).apply(
            kind, openness, new Admission(suffix.named(), suffix.test(), suffix.test())
        );
    }

    private void emit(
        final Emit emit, final Suffix suffix, final Value head,
        final List<MethodChain> chain, final List<Value> args
    ) {
        new ChainEmission(emit, this.span, head, chain, suffix).run();
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
    }
}
