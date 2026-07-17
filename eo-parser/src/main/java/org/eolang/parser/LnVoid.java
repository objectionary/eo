/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A vertical void-attribute line — R-3.4.7 / R-3.4.8 of the spec.
 *
 * <p>Form: {@code ? > name} with an optional atom-only forma-list tail
 * {@code /&#123;forma …&#125;}. The {@code ?} declares a void attribute
 * on the enclosing formation, equivalent to listing {@code name} among
 * the bracket parameters; it emits the same {@code <o name='name'
 * base='∅'/>} void child (§9.4), which {@code move-voids-up} hoists
 * among the head voids.</p>
 *
 * <p>The {@code >>} auto-name form is also accepted: {@code ? >> name}
 * declares a void whose external {@code @name} is an auto-generated
 * cactus name (unreachable from outside), while {@code name} is a
 * file-local handle (§3.10 / R-3.10.12) usable within the same
 * {@code .eo} file. A bare {@code ? >>} is a void with an auto-generated
 * name and no handle. Filling stays positional, so the auto-generated
 * external name does not affect how callers bind the void.</p>
 *
 * <p>In an atom (a formation whose head carries {@code /sig}) the void
 * may carry the forma-list tail — the formas the atom supplies as the
 * arguments of that error branch. They emit as a {@code @types}
 * attribute (R-3.4.8) whose space-separated tokens are resolved like an
 * {@code @atom} by later passes. The forma-list is rejected outside an
 * atom.</p>
 *
 * <p>{@code ? > name} and {@code ? >> name} (with the optional tail) are
 * the only shapes the {@code ?} marker may take — never an argument, a
 * method receiver, or anywhere else a value is expected. The marker is
 * therefore
 * <em>not</em> a {@link Value} kind; this line is its sole producer.
 * Cross-line behaviour: a closed leaf
 * ({@link Openness#VERTICAL_COMPLETED}), so a void has no children.</p>
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
     * @param source The source span
     */
    LnVoid(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.checkPlain(this.span, globals, emit);
        final String tail = this.span.body().substring(1);
        final int brace = tail.indexOf("/{");
        final String formas;
        final String head;
        if (brace < 0) {
            formas = "";
            head = tail;
        } else {
            formas = LnVoid.formas(tail, brace, this.span);
            head = tail.substring(0, brace);
        }
        final Suffix suffix = new Suffix(head, this.span, this.span.indent() + 1);
        final boolean shape = suffix.form() == Suffix.Form.NAME
            || suffix.form() == Suffix.Form.AUTO;
        if (!shape || suffix.constant() || suffix.atom()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a void attribute must be written as `? > name` or `? >> name`"
            );
        }
        Comments.seal(globals, emit, this.span);
        final Level level = new Transition(stack, this.span).apply(
            Kind.VOID, Openness.VERTICAL_COMPLETED, suffix.named()
        );
        if (!formas.isEmpty() && !level.patom()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a `/{…}` forma-list is allowed only inside an atom"
            );
        }
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            "∅", this.span.line(), this.span.indent()
        );
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
        }
        if (!formas.isEmpty()) {
            emit.types(formas);
        }
    }

    /**
     * Extract and validate the forma-list tail, returning
     * the space-separated formas with each leading {@code Q.} promoted
     * to {@code Φ.} (R-9.3), exactly as an {@code /sig} signature is
     * promoted.
     * @param tail The line body after the {@code ?}
     * @param brace Index of the forma-list marker in {@code tail}
     * @param span The source span (for errors)
     * @return Space-separated promoted formas
     */
    private static String formas(final String tail, final int brace, final Span span) {
        final int close = tail.indexOf('}', brace + 2);
        if (close < 0) {
            throw new ParseError(
                span.line(), span.indent(),
                "a `/{…}` forma-list must end with `}`"
            );
        }
        int after = close + 1;
        while (after < tail.length() && tail.charAt(after) == ' ') {
            after = after + 1;
        }
        if (after < tail.length()) {
            throw new ParseError(
                span.line(), span.indent(),
                "trailing garbage after a `/{…}` forma-list"
            );
        }
        return LnVoid.promoted(tail.substring(brace + 2, close), span);
    }

    /**
     * Split the forma-list body on single spaces, promoting each
     * {@code Q.}-rooted forma to {@code Φ.} and rejecting empty or
     * double-spaced entries.
     * @param inside The text inside the forma-list braces
     * @param span The source span (for errors)
     * @return Space-separated promoted formas
     */
    private static String promoted(final String inside, final Span span) {
        if (inside.isEmpty()) {
            throw new ParseError(
                span.line(), span.indent(),
                "a `/{…}` forma-list must name at least one forma"
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
                    span.line(), span.indent(),
                    "forma names in a `/{…}` list must be separated by exactly one space"
                );
            }
            if (out.length() > 0) {
                out.append(' ');
            }
            out.append(LnVoid.root(inside.substring(idx, end)));
            idx = end + 1;
        }
        return out.toString();
    }

    /**
     * Promote a leading {@code Q.} to {@code Φ.} per R-9.3; other formas
     * pass through unchanged.
     * @param forma Raw forma name
     * @return Promoted forma
     */
    private static String root(final String forma) {
        final String promoted;
        if (forma.startsWith("Q.")) {
            promoted = "Φ".concat(forma.substring(1));
        } else {
            promoted = forma;
        }
        return promoted;
    }
}
