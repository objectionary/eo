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
 *   <li>{@link Kind#HEAD} — head only, no chain, no args. Open for
 *   deeper-indent children (promotes to {@code VAPPLICATION}).</li>
 *   <li>{@link Kind#HMETHOD} — head with {@code .method} chain, 0
 *   horizontal args. Open for deeper-indent children.</li>
 *   <li>{@link Kind#HAPPLICATION} — head (with or without chain) + ≥1
 *   horizontal args. {@link Openness#HORIZONTAL_COMPLETED}.</li>
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
 * subsequent iterations. *
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
        Blanks.checkPlain(this.span, globals, emit);
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        final Value head = tokens.readValue();
        final List<MethodChain> chain = tokens.readChain();
        final List<Value> args = tokens.readArgs();
        Bindings.checkAllOrNothing(args, this.span);
        final String outer = LnApplication.readOuterBinding(tokens);
        final Suffix suffix = new Suffix(
            tokens.tail(), this.span, this.span.indent() + tokens.cursor()
        );
        if (head.kind() == Value.Kind.GROUP
            && chain.isEmpty() && args.isEmpty() && outer == null) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "redundant parentheses around a top-level expression — drop the outer `(` and `)`"
            );
        }
        Comments.attach(globals, emit, this.span, suffix.present());
        final Kind kind = LnApplication.classify(chain, args);
        final Openness openness;
        if (kind == Kind.HAPPLICATION) {
            openness = Openness.HORIZONTAL_COMPLETED;
        } else {
            openness = Openness.OPEN;
        }
        this.transition(stack, suffix, kind, openness);
        Bindings.observeChild(stack, outer, this.span);
        globals.clearBlanks();
        globals.markEmitted();
        this.emit(emit, suffix, head, chain, args);
        if (outer != null) {
            emit.slot(Emissions.bindingTag(outer));
        }
    }

    /**
     * Read an optional outer {@code :binding} that follows the line's
     * value/chain/args before the suffix — per §3.12 the binding may
     * attach to the line's whole expression when it occupies an
     * argument position (a deeper-indent child of a vapplication or
     * vertical reversed dispatch).
     * @param tokens Token reader
     * @return The binding label, or {@code null}
     */
    static String readOuterBinding(final Tokens tokens) {
        final String label;
        if (!tokens.atEnd() && tokens.current() == ':') {
            tokens.seek(tokens.cursor() + 1);
            label = tokens.readBinding();
        } else {
            label = null;
        }
        return label;
    }

    /**
     * Decide the outer kind based on chain and argument presence.
     * @param chain Method-dispatch chain (may be empty)
     * @param args Horizontal arguments (may be empty)
     * @return Outer kind
     */
    private static Kind classify(final List<MethodChain> chain, final List<Value> args) {
        final Kind kind;
        if (args.isEmpty()) {
            if (chain.isEmpty()) {
                kind = Kind.HEAD;
            } else {
                kind = Kind.HMETHOD;
            }
        } else {
            kind = Kind.HAPPLICATION;
        }
        return kind;
    }

    /**
     * Push or replace the stack level per Step B/C/D of §5.2.
     * @param stack The stack
     * @param suffix The parsed suffix
     * @param kind Initial outer kind for the pushed level
     * @param openness Initial openness for the pushed level
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private void transition(
        final Stack stack, final Suffix suffix, final Kind kind, final Openness openness
    ) {
        final Level level;
        if (stack.empty() || stack.top().indent() < this.span.indent()) {
            if (!stack.empty()
                && this.span.indent() != stack.top().indent() + 2) {
                throw new ParseError(
                    this.span.line(), 0,
                    "indent increased by more than one level"
                );
            }
            if (!stack.empty()
                && stack.top().openness() != Openness.OPEN) {
                throw new ParseError(
                    this.span.line(), 0,
                    "unexpected deeper-indent line — previous expression is closed for children"
                );
            }
            level = stack.push(
                this.span.indent(), this.span.line(), kind, openness
            );
        } else {
            level = stack.replace(this.span.line(), kind, openness);
        }
        if (suffix.present()) {
            level.name();
        }
    }

    /**
     * Emit the head, chain (flat siblings), and horizontal args
     * (children of the head's {@code <o>} or last chain link). The last
     * {@code <o>} (head if no chain, last link if chain) remains open;
     * cursor stays inside.
     * @param emit The directives sink
     * @param suffix The parsed suffix
     * @param head The head value
     * @param chain The chain links (may be empty)
     * @param args The horizontal arguments (may be empty)
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    private void emit(
        final Emit emit, final Suffix suffix, final Value head,
        final List<MethodChain> chain, final List<Value> args
    ) {
        final String name = suffix.attribute(this.span.line(), this.span.indent());
        if (chain.isEmpty()) {
            Emissions.openValue(emit, name, head, this.span.line());
            if (suffix.constant()) {
                emit.constant();
            }
        } else {
            Emissions.openValue(emit, null, head, this.span.line());
            emit.close();
            for (int idx = 0; idx < chain.size() - 1; idx = idx + 1) {
                final MethodChain link = chain.get(idx);
                emit.object(null, ".".concat(link.name()), this.span.line(), link.dot());
                emit.method();
                emit.close();
            }
            final MethodChain last = chain.get(chain.size() - 1);
            emit.object(
                name, ".".concat(last.name()),
                this.span.line(), last.dot()
            );
            emit.method();
            if (suffix.constant()) {
                emit.constant();
            }
        }
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
    }
}
