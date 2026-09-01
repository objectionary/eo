/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.regex.Pattern;

/**
 * A vertical void-attribute line — R-3.4.7 / R-3.4.8 of the spec.
 *
 * <p>Form: {@code ? > name} with an optional atom-only type annotation.
 * The {@code ?} declares a void attribute on the enclosing formation,
 * equivalent to listing {@code name} among the bracket parameters; it
 * emits the same empty-set-based void child named {@code name}
 * (§9.4).</p>
 *
 * <p>The {@code >>} auto-name form is also accepted: {@code ? >> name}
 * declares a void whose external {@code @name} is an auto-generated
 * cactus name (unreachable from outside), while {@code name} is a
 * file-local handle (§3.10 / R-3.10.12) usable within the same
 * {@code .eo} file. A bare {@code ? >>} is a void with an auto-generated
 * name and no handle. Filling stays positional, so the auto-generated
 * external name does not affect how callers bind the void.</p>
 *
 * <p>Inside an atom (a formation whose head carries {@code /sig}) the
 * void may carry exactly one type annotation (R-3.4.8):</p>
 *
 * <ul>
 * <li>{@code /type} — the void's own type: a concrete forma or a
 * generic type variable ({@code A}–{@code F}), with an optional
 * trailing {@code ?} marking a maybe-terminator value. Emits
 * {@code @type}.</li>
 * <li>{@code /{type …}} — the void is a callback formation;
 * the brace list gives the types of its void parameters (the arguments
 * the atom supplies to that branch). No {@code ?} inside. Emits
 * {@code @args}.</li>
 * </ul>
 *
 * <p>A concrete forma has a leading {@code Q.} promoted to {@code Φ.};
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
 * <p>{@code ? > name}, {@code ? >> name} and {@code ? > ^} — each
 * optionally followed by one type annotation — are the only shapes the
 * {@code ?} marker may take, never an argument, a method receiver, or
 * anywhere else a value is expected. The marker is therefore <em>not</em> a {@link Value}
 * kind; this line is its sole producer. Cross-line behaviour: a closed
 * leaf ({@link Openness#VCOMPLETED}), so a void has no children.</p>
 *
 * @since 0.1
 */
final class LnVoid implements Line {

    /**
     * The shape of a head that declares the formation's receiver.
     */
    private static final Pattern RECEIVER = Pattern.compile(" > \\^ *");

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
        final String tail = this.span.body().substring(1);
        final int slash = tail.indexOf('/');
        if (LnVoid.RECEIVER.matcher(LnVoid.head(tail, slash)).matches()) {
            this.receiver(stack, globals, emit, slash);
        } else {
            this.attribute(stack, globals, emit, tail, slash);
        }
        this.annotate(emit, tail, slash);
    }

    private void receiver(
        final Stack stack, final Globals globals, final Emit emit, final int slash
    ) {
        globals.seal(emit, this.span);
        this.checkTyped(
            new Transition(stack, this.span).apply(
                Kind.VOID, Openness.VCOMPLETED, new Admission("^", true)
            ),
            slash
        );
        globals.clearBlanks();
        globals.markEmitted();
        emit.object("ρ", "∅", this.span.line(), this.span.indent());
    }

    private void attribute(
        final Stack stack, final Globals globals, final Emit emit,
        final String tail, final int slash
    ) {
        final Suffix suffix = new Suffix(
            LnVoid.head(tail, slash), this.span, this.span.indent() + 1
        );
        if (suffix.form() != Suffix.Form.NAME && suffix.form() != Suffix.Form.AUTO
            || suffix.constant()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a void attribute must be written as `? > name` or `? >> name`"
            );
        }
        globals.seal(emit, this.span);
        this.checkTyped(
            new Transition(stack, this.span).apply(
                Kind.VOID, Openness.VCOMPLETED, new Admission(suffix.named(), true)
            ),
            slash
        );
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            "∅", this.span.line(), this.span.indent()
        );
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
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
            && tail.charAt(idx) != '?'
            && tail.charAt(idx) != '/') {
            idx = idx + 1;
        }
        if (idx == begin) {
            throw new ParseError(
                span.line(), span.indent(),
                "a void type annotation requires a type"
            );
        }
        final String base = Suffix.typeAtom(
            tail.substring(begin, idx), span, span.indent() + 1 + begin
        );
        final String result;
        if (idx < tail.length() && tail.charAt(idx) == '?') {
            LnVoid.endsClean(tail, idx + 1, span);
            result = base.concat("?");
        } else {
            LnVoid.endsClean(tail, idx, span);
            result = base;
        }
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
            out.append(Suffix.typeAtom(member, span, span.indent() + 1 + offset + idx));
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
