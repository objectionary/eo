/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A vertical void-attribute line — R-3.4.7 / R-3.4.8 of the spec.
 *
 * <p>Form: {@code ? > name} with an optional type annotation.
 * The {@code ?} declares a void attribute on the enclosing formation,
 * equivalent to listing {@code name} among the bracket parameters; it
 * emits the same empty-set-based void child named {@code name} (§9.4),
 * which {@code move-voids-up} hoists among the head voids.</p>
 *
 * <p>The {@code >>} auto-name form is also accepted: {@code ? >> name}
 * declares a void whose external {@code @name} is an auto-generated
 * cactus name (unreachable from outside), while {@code name} is a
 * file-local handle (§3.10 / R-3.10.12) usable within the same
 * {@code .eo} file. A bare {@code ? >>} is a void with an auto-generated
 * name and no handle. Filling stays positional, so the auto-generated
 * external name does not affect how callers bind the void.</p>
 *
 * <p>A void may carry exactly one type annotation (R-3.4.8) — a union
 * of the types it may hold, written {@code /one|two|three} with no
 * spaces around the pipes, and closed by an optional {@code ?} marking
 * the whole union maybe-bottom. A member is either:</p>
 *
 * <ul>
 *   <li>a concrete forma or a generic type variable
 *   ({@code A}–{@code F});</li>
 *   <li>a formation type {@code &#123;type …&#125;} naming the types of
 *   that formation's voids — {@code &#123;&#125;} for one with none,
 *   the shape a callback branch or an anonymous empty object takes. No
 *   {@code ?} inside.</li>
 * </ul>
 *
 * <p>A concrete forma has a leading {@code Q.} promoted to {@code Φ.};
 * a generic type variable stays verbatim. The whole union lands in one
 * {@code @type} attribute, members separated by a single space, a
 * formation member keeping its braces.</p>
 *
 * <p>{@code ? > name} and {@code ? >> name} — each optionally followed
 * by one type annotation — are the only shapes the {@code ?} marker may
 * take, never an argument, a method receiver, or anywhere else a value
 * is expected. The marker is therefore <em>not</em> a {@link Value}
 * kind; this line is its sole producer. Cross-line behaviour: a closed
 * leaf ({@link Openness#VERTICAL_COMPLETED}), so a void has no
 * children.</p>
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
        final int slash = tail.indexOf('/');
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
        Comments.seal(globals, emit, this.span);
        new Transition(stack, this.span).apply(
            Kind.VOID, Openness.VERTICAL_COMPLETED, suffix.named()
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
        this.annotate(emit, tail, slash);
    }

    /**
     * The {@code ? > name} head, before any {@code /} type annotation.
     * @param tail The line body after the {@code ?}
     * @param slash Index of the first {@code /} in {@code tail}, or -1
     * @return The head substring
     */
    private static String head(final String tail, final int slash) {
        final String head;
        if (slash < 0) {
            head = tail;
        } else {
            head = tail.substring(0, slash);
        }
        return head;
    }

    /**
     * Emit the void's type annotation (R-3.4.8), if any, as the one
     * {@code @type} attribute holding the whole union.
     * @param emit The directives sink
     * @param tail The line body after the {@code ?}
     * @param slash Index of the first {@code /} in {@code tail}, or -1
     */
    private void annotate(final Emit emit, final String tail, final int slash) {
        if (slash >= 0) {
            emit.type(this.union(tail, slash));
        }
    }

    /**
     * Parse the union that follows the {@code /} marker (R-3.4.8): one
     * or more members separated by {@code |}, closed by an optional
     * {@code ?} that marks the whole union maybe-bottom. Members come
     * out single-space separated, in source order.
     * @param tail The line body after the {@code ?}
     * @param slash Index of the {@code /} marker in {@code tail}
     * @return The promoted {@code @type} value, with {@code ?} preserved
     */
    private String union(final String tail, final int slash) {
        final StringBuilder out = new StringBuilder(tail.length());
        int idx = slash + 1;
        boolean more = true;
        while (more) {
            if (out.length() > 0) {
                out.append(' ');
            }
            idx = this.member(tail, idx, out);
            more = idx < tail.length() && tail.charAt(idx) == '|';
            if (more) {
                idx = idx + 1;
            }
        }
        final String result;
        if (idx < tail.length() && tail.charAt(idx) == '?') {
            LnVoid.endsClean(tail, idx + 1, this.span);
            result = out.toString().concat("?");
        } else {
            LnVoid.endsClean(tail, idx, this.span);
            result = out.toString();
        }
        return result;
    }

    /**
     * Parse one union member — a formation type when it opens with
     * {@code &#123;}, a single type atom otherwise — appending it to
     * {@code out}.
     * @param tail The line body after the {@code ?}
     * @param from Index the member starts at
     * @param out Sink for the promoted member
     * @return Index just past the member
     */
    private int member(final String tail, final int from, final StringBuilder out) {
        final int next;
        if (from < tail.length() && tail.charAt(from) == '{') {
            next = this.formation(tail, from, out);
        } else {
            next = this.forma(tail, from, out);
        }
        return next;
    }

    /**
     * Parse a formation member {@code &#123;type …&#125;} (R-3.4.8): the
     * types of that formation's voids, single-space separated, none for
     * {@code &#123;&#125;}, no {@code ?} inside.
     * @param tail The line body after the {@code ?}
     * @param from Index of the opening brace
     * @param out Sink for the promoted member
     * @return Index just past the closing brace
     */
    private int formation(final String tail, final int from, final StringBuilder out) {
        final int close = tail.indexOf('}', from + 1);
        if (close < 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a `/{…}` formation type must end with `}`"
            );
        }
        out.append('{')
            .append(LnVoid.voids(tail.substring(from + 1, close), this.span))
            .append('}');
        return close + 1;
    }

    /**
     * Parse a bare member: one type atom, a generic variable verbatim or
     * a concrete forma promoted from {@code Q.} to {@code Φ.}.
     * @param tail The line body after the {@code ?}
     * @param from Index the atom starts at
     * @param out Sink for the promoted atom
     * @return Index just past the atom
     */
    private int forma(final String tail, final int from, final StringBuilder out) {
        int idx = from;
        while (idx < tail.length() && !LnVoid.breaks(tail.charAt(idx))) {
            idx = idx + 1;
        }
        if (idx == from) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a void type annotation requires a type"
            );
        }
        out.append(
            Suffix.typeAtom(
                tail.substring(from, idx), this.span, this.span.indent() + 1 + from
            )
        );
        return idx;
    }

    /**
     * Whether the character ends a bare union member.
     * @param symbol The character
     * @return True if the member stops here
     */
    private static boolean breaks(final char symbol) {
        return symbol == ' ' || symbol == '?' || symbol == '/' || symbol == '|';
    }

    /**
     * Split the void types of a formation member on single spaces,
     * promoting each atom and rejecting double spaces and the {@code ?}
     * optional marker. An empty list is a formation with no voids.
     * @param inside The text inside the braces
     * @param span The source span (for errors)
     * @return The space-separated promoted types, empty for no voids
     */
    private static String voids(final String inside, final Span span) {
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
                    "types in a `/{…}` list must be separated by exactly one space"
                );
            }
            final String member = inside.substring(idx, end);
            if (member.indexOf('?') >= 0) {
                throw new ParseError(
                    span.line(), span.indent(),
                    "? is not allowed inside a /{…} formation type"
                );
            }
            if (out.length() > 0) {
                out.append(' ');
            }
            out.append(Suffix.typeAtom(member, span, span.indent()));
            idx = end + 1;
        }
        return out.toString();
    }

    /**
     * Verify the rest of {@code tail} from {@code from} onward is only
     * whitespace; a further {@code /} is a second annotation, anything
     * else is trailing garbage.
     * @param tail The line body after the {@code ?}
     * @param from Index after the consumed annotation
     * @param span The source span (for errors)
     */
    private static void endsClean(final String tail, final int from, final Span span) {
        int idx = from;
        while (idx < tail.length() && tail.charAt(idx) == ' ') {
            idx = idx + 1;
        }
        if (idx < tail.length()) {
            final String message;
            if (tail.charAt(idx) == '/') {
                message = "a void attribute may carry at most one type annotation";
            } else if (tail.charAt(idx) == '|') {
                message = "the optional marker ? must close a void type annotation";
            } else {
                message = "trailing garbage after a void type annotation";
            }
            throw new ParseError(span.line(), span.indent(), message);
        }
    }
}
