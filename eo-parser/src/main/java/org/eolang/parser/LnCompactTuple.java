/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * A compact-tuple line — §3.9 of the spec.
 *
 * <p>Form: {@code head *N [> name]}. The head is parsed exactly like
 * an {@link LnApplication} head (identifier, root, literal, optional
 * dotted chain — paren groups and reversed bases will land in later
 * rounds). After the head, a single space and a {@code *} followed by
 * an optional non-negative integer {@code N} (default 0) declares the
 * compact-tuple marker (R-3.9.1).</p>
 *
 * <p>Cross-line semantics (R-3.9.2):</p>
 *
 * <ul>
 * <li>The first {@code N} deeper-indent children stay as direct
 * positional args of the head.</li>
 * <li>The {@code (N+1)}-th and later children are wrapped in a
 * synthesised {@code <o base='Φ.tuple' star=''>} appended as the
 * <em>last</em> child of the head.</li>
 * </ul>
 *
 * <p>The wrapper open/close is handled by the {@link Stack.Opener} /
 * {@link Stack.Closer} hooks in {@link Eo}; this class only parses the
 * line, pushes a {@link Kind#COMPACT_TUPLE} level with the parsed N,
 * and emits the head's {@code <o>}.</p>
 *
 * <p>Close-time check R-5.3.3: {@code children >= N}. Fires from
 * {@code Eo.checkOnClose}.</p>
 *
 * <p>This iteration accepts identifier and root heads (with optional
 * chains). Reversed-dispatch heads ({@code joined. *1 > sixth}) are
 * dispatched via the reversed-classifier path and are not yet
 * supported.</p>
 *
 * @since 0.1
 */
final class LnCompactTuple implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnCompactTuple(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        final Value head = tokens.readValue();
        final List<MethodChain> chain;
        if (head.chainable()) {
            chain = tokens.readChain();
        } else {
            chain = new ArrayList<>(0);
        }
        if (tokens.atEnd() || tokens.current() != ' ') {
            throw new ParseError(
                this.span.line(), this.span.indent() + tokens.cursor(),
                "compact tuple marker must follow a space"
            );
        }
        tokens.seek(tokens.cursor() + 1);
        if (tokens.atEnd() || tokens.current() != '*') {
            throw new ParseError(
                this.span.line(), this.span.indent() + tokens.cursor(),
                "compact tuple marker must start with `*`"
            );
        }
        tokens.seek(tokens.cursor() + 1);
        final int count = LnCompactTuple.readCount(tokens, this.span);
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
        final Level level = this.transition(stack, suffix);
        level.compact(count);
        globals.clearBlanks();
        globals.markEmitted();
        new ChainEmission(emit, this.span, head, chain, suffix).run();
    }

    private Level transition(final Stack stack, final Suffix suffix) {
        return new Transition(stack, this.span).apply(
            Kind.COMPACT_TUPLE, Openness.OPEN,
            new Admission(suffix.named(), suffix.test(), suffix.test())
        );
    }

    private static int readCount(final Tokens tokens, final Span span) {
        long count = 0;
        final int start = tokens.cursor();
        while (!tokens.atEnd()) {
            final char glyph = tokens.current();
            if (glyph < '0' || glyph > '9') {
                break;
            }
            count = count * 10 + glyph - '0';
            if (count > Integer.MAX_VALUE) {
                throw new ParseError(
                    span.line(), span.indent() + start,
                    "compact tuple count is too large"
                );
            }
            tokens.seek(tokens.cursor() + 1);
        }
        if (tokens.cursor() - start > 1 && tokens.body().charAt(start) == '0') {
            throw new ParseError(
                span.line(), span.indent() + start,
                "integer literal must not have leading zeros"
            );
        }
        return (int) count;
    }
}
