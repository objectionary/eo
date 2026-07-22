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
 * is named by the right-hand suffix. The compact test shorthand
 * {@code lhs ++> name} (R-3.10.8 / R-6.3.6) is accepted as sugar for
 * {@code lhs > [] +> name} — a parameterless test attribute whose sole
 * binding is the {@code φ} decoratee {@code lhs}. The throwing
 * counterpart {@code lhs --> name} is sugar for {@code lhs > [] -> name}.
 * </p>
 *
 * <p>Mechanics (R-3.10.1):</p>
 *
 * <ul>
 *   <li>LHS is parsed as an application expression (head + optional
 *   chain + optional hargs) or a reversed dispatch ({@code if.}) via
 *   {@link Emissions#expression}. Its outermost {@code <o>} carries
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
 * accepted. An unnamed deeper-indent line is a vertical argument to φ; a
 * named one closes the φ and becomes a sibling attribute of the
 * formation (§4.5), so an only-phi formation may carry named attributes
 * besides its φ decoratee. The {@link Stack} opener closes the φ before
 * such a named child (see {@link Eo}).</p>
 *
 * <p>A compact-tuple LHS (R-3.9.1 + R-3.10.6) — a head with a trailing
 * {@code *N} marker, e.g. {@code seq * > [m]} — keeps the φ
 * {@link Openness#OPEN} and flags the level {@link Level#star()}, so its
 * deeper-indent lines are absorbed into a {@code Φ.tuple} as §3.9 does
 * for a bare {@link LnCompactTuple} rather than {@link #bare(Tokens)}
 * reading the {@code *} as a completed empty-tuple argument.</p>
 *
 * <p>This iteration accepts identifier and root LHS heads with
 * optional chains and identifier / INT / STAR / STRING / FLOAT /
 * ROOT horizontal args. R-3.10.6 LHS restrictions are honoured by
 * scanner exclusion (formations and reversed-with-hargs LHS are not
 * accepted as inputs because their classifiers fire first). *
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnnecessaryLocalRule"})
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
        final String lhs;
        final List<String> params;
        final Suffix suffix;
        final int origin;
        if (phi >= 0) {
            final int bracket = phi + 2;
            final int close = body.indexOf(']', bracket);
            if (close < 0) {
                throw new ParseError(
                    this.span.line(), this.span.indent() + bracket,
                    "only-phi parameter list missing closing `]`"
                );
            }
            lhs = body.substring(0, phi).stripTrailing();
            params = LnOnlyPhi.parseParams(
                body.substring(bracket + 1, close), this.span, bracket + 1
            );
            suffix = new Suffix(
                body.substring(close + 1), this.span, this.span.indent() + close + 1
            );
            origin = bracket + 1;
        } else {
            final int shorthand = LnOnlyPhi.shorthandArrow(body);
            if (shorthand < 0) {
                throw new ParseError(
                    this.span.line(), this.span.indent(),
                    "only-phi formation must contain `> [`, `++>` or `-->`"
                );
            }
            lhs = body.substring(0, shorthand).stripTrailing();
            params = new ArrayList<>(0);
            suffix = new Suffix(
                body.substring(shorthand + 1), this.span, this.span.indent() + shorthand + 1
            );
            origin = shorthand;
        }
        if (lhs.isEmpty()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "only-phi formation requires a non-empty body before `> [` or `++>`"
            );
        }
        if (suffix.test()) {
            Blanks.checkTest(this.span, globals, emit);
        }
        final Span inner = new Span(
            " ".repeat(this.span.indent()).concat(lhs), this.span.line()
        );
        Comments.seal(globals, emit, this.span);
        final Tokens tokens = this.slot(stack, suffix, inner);
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            null, this.span.line(), this.span.indent()
        );
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
        }
        if (suffix.constant()) {
            emit.constant();
        }
        this.emitVoids(emit, params, origin);
        final boolean open = stack.top().openness() == Openness.OPEN;
        if (open) {
            stack.top().openPhi();
        }
        this.emitPhi(emit, tokens, open);
    }

    /**
     * Parse the LHS into the φ token reader and push the formation
     * level, detecting a trailing compact-tuple marker {@code *N}
     * (R-3.9.1 + R-3.10.6) that keeps the φ {@link Openness#OPEN} and
     * flags the level {@link Level#star()} for tuple elements.
     * @param stack The stack
     * @param suffix Right-hand-side suffix
     * @param inner The LHS as an indent-aligned span
     * @return The φ token reader rewound to the head
     */
    private Tokens slot(final Stack stack, final Suffix suffix, final Span inner) {
        final int stars = LnOnlyPhi.compactStar(inner.body());
        final Tokens tokens = LnOnlyPhi.reader(inner, stars);
        final boolean open = stars >= 0 || LnOnlyPhi.bare(tokens);
        tokens.seek(0);
        final Level level = this.transition(stack, suffix, open);
        if (stars >= 0) {
            level.compact(stars);
            level.markStar();
        }
        return tokens;
    }

    /**
     * The top-level index of the compact test shorthand on an inline-phi
     * line — the truthy {@code ++>} or, failing that, the throwing
     * {@code -->} marker — or -1 when neither is present.
     * @param body The line body
     * @return Index of the shorthand marker, or -1
     */
    private static int shorthandArrow(final String body) {
        int idx = Eo.topLevelPlusPlusArrowIndex(body);
        if (idx < 0) {
            idx = Eo.topLevelMinusMinusArrowIndex(body);
        }
        return idx;
    }

    /**
     * Emit the only-phi void parameters as {@code ∅}-based children,
     * mapping {@code @} to {@code φ} (R-3.4.2 / R-9.3) and advancing the
     * source column across each name and its separating space.
     * @param emit Emitter
     * @param params Parameter names in source order
     * @param origin Source column of the first parameter
     */
    private void emitVoids(final Emit emit, final List<String> params, final int origin) {
        int column = this.span.indent() + origin;
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
    }

    /**
     * Emit the LHS as the formation's {@code φ} slot via
     * {@link Emissions#expression} — which handles a head + chain, or a
     * reversed dispatch ({@code if.}), leaving the outermost {@code <o>}
     * open. When {@code open} that φ stays on the cursor so deeper-indent
     * lines attach to it as vertical arguments (the {@link Stack.Closer}
     * closes it and the formation); otherwise its horizontal args are
     * complete and it is closed here.
     * @param emit Emitter
     * @param tokens Token reader rewound to the LHS head
     * @param open Whether the φ stays open for vertical arguments
     */
    private void emitPhi(final Emit emit, final Tokens tokens, final boolean open) {
        Emissions.expression(emit, "φ", tokens, this.span.line());
        if (!open) {
            emit.close();
        }
    }

    /**
     * Whether the only-phi LHS carries no horizontal args, so its φ
     * stays {@link Openness#OPEN} for deeper-indent vertical arguments
     * (§4.5); otherwise the φ is a full application and the formation is
     * {@link Openness#HORIZONTAL_COMPLETED}. The LHS may be a reversed
     * dispatch ({@code if. > [t] >> rec}), whose trailing dot is skipped
     * exactly as {@link Emissions#expression} does so both agree on the
     * arg boundary. Consumes the token stream; callers rewind before
     * emitting.
     * @param tokens Token reader positioned at the LHS head
     * @return True if the φ has no horizontal args
     */
    private static boolean bare(final Tokens tokens) {
        if (LnOnlyPhi.reversedAhead(tokens, tokens.readValue())) {
            tokens.seek(tokens.cursor() + 1);
        } else {
            tokens.readChain();
        }
        return tokens.readArgs().isEmpty();
    }

    /**
     * Whether the cursor sits at a reversed-dispatch dot after the head
     * — an identifier head immediately followed by a {@code .} that ends
     * the body or precedes a space (§3.8). Mirrors
     * {@link Emissions#expression} so the arg boundary agrees.
     * @param tokens Token reader positioned after the head
     * @param head The just-read head value
     * @return True when a reversed-dispatch dot follows the head
     */
    private static boolean reversedAhead(final Tokens tokens, final Value head) {
        final boolean result;
        if (head.kind() == Value.Kind.IDENTIFIER
            && !tokens.atEnd() && tokens.current() == '.') {
            final int probe = tokens.cursor() + 1;
            result = probe >= tokens.body().length()
                || tokens.body().charAt(probe) == ' ';
        } else {
            result = false;
        }
        return result;
    }

    /**
     * Push or replace the stack level per Step B/C/D of §5.2. A bare
     * (zero-hargs) φ opens the level for vertical arguments; a φ that
     * already carries horizontal args is horizontally completed.
     * @param stack The stack
     * @param suffix Right-hand-side suffix
     * @param open Whether the φ has no horizontal args
     * @return The pushed-or-replaced level
     */
    private Level transition(final Stack stack, final Suffix suffix, final boolean open) {
        final Openness openness;
        if (open) {
            openness = Openness.OPEN;
        } else {
            openness = Openness.HORIZONTAL_COMPLETED;
        }
        return new Transition(stack, this.span).apply(
            Kind.ONLY_PHI_FORMATION, openness, suffix.named()
        );
    }

    /**
     * The {@code N} of a trailing compact-tuple marker {@code *N} on the
     * inline-phi LHS ({@code head *N}, R-3.9.1 + R-3.10.6), or -1 when
     * the LHS is not a compact-tuple head — its head must be a single
     * space-free token that is not a reversed dispatch (no trailing dot).
     * @param lhs The LHS body
     * @return The N count, or -1 when not a compact-tuple head
     */
    private static int compactStar(final String lhs) {
        final int space = lhs.indexOf(' ');
        final int result;
        if (space > 0 && lhs.charAt(space - 1) != '.'
            && space + 1 < lhs.length() && lhs.charAt(space + 1) == '*') {
            result = LnOnlyPhi.starCount(lhs, space + 2);
        } else {
            result = -1;
        }
        return result;
    }

    /**
     * The φ token reader: the whole LHS when {@code stars} is -1, or the
     * head with the trailing {@code *N} marker stripped (up to the first
     * space) when a compact tuple was detected.
     * @param inner The LHS as an indent-aligned span
     * @param stars The compact-tuple N, or -1
     * @return A fresh token reader over the φ head
     */
    private static Tokens reader(final Span inner, final int stars) {
        final String lhs = inner.body();
        final String head;
        if (stars < 0) {
            head = lhs;
        } else {
            head = lhs.substring(0, lhs.indexOf(' '));
        }
        final Span span = new Span(
            " ".repeat(inner.indent()).concat(head), inner.line()
        );
        return new Tokens(span.body(), span);
    }

    /**
     * The non-negative integer {@code N} spelled from {@code from} to the
     * end of {@code lhs} (0 when empty, R-3.9.1), or -1 when any character
     * is not a digit (which also rejects trailing horizontal arguments).
     * @param lhs The LHS body
     * @param from Index of the first character after {@code *}
     * @return The N count, or -1 when the tail is not all digits
     */
    private static int starCount(final String lhs, final int from) {
        int count = 0;
        boolean digits = true;
        for (int idx = from; idx < lhs.length(); idx = idx + 1) {
            final char glyph = lhs.charAt(idx);
            if (glyph >= '0' && glyph <= '9') {
                count = count * 10 + glyph - '0';
            } else {
                digits = false;
            }
        }
        final int result;
        if (digits) {
            result = count;
        } else {
            result = -1;
        }
        return result;
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
