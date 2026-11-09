/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A vertical void-attribute line — R-3.4.7 / R-3.4.8 of the spec.
 *
 * <p>Form: {@code ? > name} with an optional atom-only type annotation.
 * The {@code ?} declares a void attribute on the enclosing formation,
 * equivalent to listing {@code name} among the bracket parameters; it
 * emits the same empty-set-based void child named {@code name}
 * (§9.4).</p>
 *
 * <p>Inside an atom (a formation whose head carries {@code /sig}) the
 * void may carry exactly one type annotation (R-3.4.8):</p>
 *
 * <ul>
 * <li>{@code /type} — the void's own type, a {@link TypeTree} of
 * concrete formas and generic type variables ({@code A}–{@code F}),
 * with an optional trailing {@code ?} marking a maybe-terminator value.
 * Emits {@code @type}.</li>
 * <li>{@code /{type …}} — the void is a callback formation;
 * the brace list gives the types of its void parameters (the arguments
 * the atom supplies to that branch), each a {@link TypeTree}. No
 * {@code ?} inside. Emits {@code @args}.</li>
 * </ul>
 *
 * <p>Every {@code Q.} root inside a type is promoted to {@code Φ.};
 * a generic type variable stays verbatim. Both annotation forms are
 * rejected outside an atom.</p>
 *
 * <p>The name may also be {@code ^}, which declares the formation's
 * receiver and emits a void named {@code ρ} (R-3.4.11). Its position
 * among the voids is free — a dispatch looks it up by name, not by
 * position — so it obeys the same R-3.4.9 ordering as every other void:
 * it may not stand below an attribute the formation has already bound.
 * It carries a type annotation like any other void, which is to say
 * inside an atom only.</p>
 *
 * <p>Inside an atom, whose bracket head stays empty (R-3.4.10), the name
 * may instead be {@code @}, the only spelling left for a φ void there;
 * it maps to {@code φ} per R-3.4.2 / R-9.3, the same way {@code ^} maps
 * to {@code ρ}.</p>
 *
 * <p>{@code ? > name}, {@code ? > ^} and {@code ? > @} — each optionally
 * followed by one type annotation — are the only shapes the {@code ?}
 * marker may take, never an argument, a method receiver, or anywhere else
 * a value is expected. The {@code >>} auto-name is refused, with or without
 * a handle: a caller fills a void by its name, and no caller could name a
 * void the compiler named (R-3.4.7). The marker is therefore <em>not</em> a
 * {@link Value} kind; this line is its sole producer. Cross-line behaviour:
 * a closed leaf ({@link Openness#VCOMPLETED}), so a void has no children.</p>
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
     *
     * @param source The source span
     */
    LnVoid(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.checkPlain(this.span, globals, emit);
        final String tail = this.span.body().substring(1);
        final int slash = tail.indexOf('/');
        this.attribute(
            stack, globals, emit,
            new Suffix(LnVoid.head(tail, slash), this.span, this.span.indent() + 1),
            slash
        );
        this.annotate(emit, tail, slash);
    }

    private void attribute(
        final Stack stack, final Globals globals, final Emit emit,
        final Suffix suffix, final int slash
    ) {
        if (suffix.form() == Suffix.Form.NONE || suffix.auto() || suffix.test()
            || suffix.constant()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a void attribute must be written as `? > name`"
            );
        }
        globals.seal(emit, this.span);
        final Level level = new Transition(stack, this.span).apply(
            Kind.VOID, Openness.VCOMPLETED, new Admission(suffix.named(), true)
        );
        this.checkPlaced(level);
        this.checkTyped(level, slash);
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(this.name(suffix), "∅", this.span.line(), this.span.indent());
    }

    private String name(final Suffix suffix) {
        final String result;
        if (suffix.form() == Suffix.Form.RECEIVER) {
            result = new VoidName(suffix.named()).asString();
        } else {
            result = suffix.attribute(this.span.line(), this.span.indent());
        }
        return result;
    }

    private void checkPlaced(final Level level) {
        if (!level.parent().formation()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a void attribute is legal only as a direct child of a formation"
            );
        }
    }

    private void checkTyped(final Level level, final int slash) {
        if (slash >= 0 && !level.patom()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a void type annotation is allowed only inside an atom"
            );
        }
    }

    private static String head(final String tail, final int slash) {
        final String head;
        if (slash < 0) {
            head = tail;
        } else {
            head = tail.substring(0, slash);
        }
        return head;
    }

    private void annotate(final Emit emit, final String tail, final int slash) {
        if (slash >= 0) {
            if (slash + 1 < tail.length() && tail.charAt(slash + 1) == '{') {
                emit.args(LnVoid.args(tail, slash, this.span));
            } else {
                emit.type(LnVoid.type(tail, slash, this.span));
            }
        }
    }

    private static String type(final String tail, final int slash, final Span span) {
        int idx = slash + 1;
        final int begin = idx;
        while (idx < tail.length()
            && tail.charAt(idx) != ' '
            && tail.charAt(idx) != '/') {
            idx = idx + 1;
        }
        final String raw = tail.substring(begin, idx);
        if (raw.isEmpty() || "?".equals(raw)) {
            throw new ParseError(
                span.line(), span.indent(),
                "a void type annotation requires a type"
            );
        }
        final String result = new TypeTree(
            raw, tail.substring(idx), span, span.indent() + 1 + begin
        ).asString();
        LnVoid.endsClean(tail, idx, span);
        return result;
    }

    private static String args(final String tail, final int slash, final Span span) {
        final int open = slash + 1;
        final int close = tail.indexOf('}', open + 1);
        if (close < 0) {
            throw new ParseError(
                span.line(), span.indent(),
                "a `/{…}` argument list must end with `}`"
            );
        }
        LnVoid.endsClean(tail, close + 1, span);
        return LnVoid.members(tail.substring(open + 1, close), open + 1, span);
    }

    private static String members(final String inside, final int offset, final Span span) {
        if (inside.isEmpty()) {
            throw new ParseError(
                span.line(), span.indent() + 1 + offset,
                "a `/{…}` argument list must name at least one type"
            );
        }
        final StringBuilder out = new StringBuilder(inside.length());
        int idx = 0;
        while (idx < inside.length()) {
            int end = idx;
            while (end < inside.length() && inside.charAt(end) != ' ') {
                end = end + 1;
            }
            if (end == idx) {
                throw new ParseError(
                    span.line(), span.indent() + 1 + offset + idx,
                    "types in a /{…} list must be separated by exactly one space"
                );
            }
            final String member = inside.substring(idx, end);
            if (member.indexOf('?') >= 0) {
                throw new ParseError(
                    span.line(), span.indent() + 1 + offset + idx,
                    "? is not allowed inside a /{…} argument list"
                );
            }
            if (out.length() > 0) {
                out.append(' ');
            }
            out.append(
                new TypeTree(
                    member, inside.substring(end), span, span.indent() + 1 + offset + idx
                ).asString()
            );
            idx = end + 1;
            if (idx == inside.length()) {
                throw new ParseError(
                    span.line(), span.indent() + 1 + offset + idx,
                    "types in a /{…} list must be separated by exactly one space"
                );
            }
        }
        return out.toString();
    }

    private static void endsClean(final String tail, final int from, final Span span) {
        int idx = from;
        while (idx < tail.length() && tail.charAt(idx) == ' ') {
            idx = idx + 1;
        }
        if (idx < tail.length()) {
            final String message;
            if (tail.charAt(idx) == '/') {
                message = "a void attribute may carry at most one type annotation";
            } else {
                message = "trailing garbage after a void type annotation";
            }
            throw new ParseError(span.line(), span.indent(), message);
        }
    }
}
