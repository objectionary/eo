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
 * accepted. An only-phi argument may not carry a name suffix (the
 * formation binds only φ); the {@link Stack} flags such arguments and
 * the close-time check in {@link Eo} rejects a name on them.</p>
 *
 * <p>A compact-tuple LHS (R-3.9.1 + R-3.10.6) — a head with a trailing
 * {@code *N} marker, e.g. {@code seq * > [m]} — is a further open case:
 * the φ stays {@link Openness#OPEN} and its deeper-indent lines are
 * absorbed into a synthesised {@code Φ.tuple} exactly as §3.9 does for a
 * bare {@link LnCompactTuple}, rather than {@link #bare(Tokens)} reading
 * the {@code *} as a completed empty-tuple horizontal argument. The
 * level is flagged {@link Level#star()} and reuses the {@code Φ.tuple}
 * wrapper hooks in {@link Eo}.</p>
 *
 * <p>This iteration accepts identifier and root LHS heads with
 * optional chains and identifier / INT / STAR / STRING / FLOAT /
 * ROOT horizontal args. R-3.10.6 LHS restrictions are honoured by
 * scanner exclusion (formations and reversed-with-hargs LHS are not
 * accepted as inputs because their classifiers fire first). *
 *
 * @since 0.1
 */
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
        final int marker = LnOnlyPhi.compactMarker(new Tokens(inner.body(), inner));
        final Tokens tokens;
        final boolean open;
        if (marker < 0) {
            tokens = new Tokens(inner.body(), inner);
            open = LnOnlyPhi.bare(tokens);
            tokens.seek(0);
        } else {
            final Span head = new Span(
                " ".repeat(this.span.indent())
                    .concat(inner.body().substring(0, marker)),
                this.span.line()
            );
            tokens = new Tokens(head.body(), head);
            open = true;
        }
        Comments.seal(globals, emit, this.span);
        final Level level = this.transition(stack, suffix, open);
        if (marker >= 0) {
            level.compact(LnOnlyPhi.starCount(inner.body(), marker));
            level.markStar();
        }
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
        this.emitPhi(emit, tokens, open);
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
     * The index in the LHS of the space that begins a trailing
     * compact-tuple marker {@code *N} (R-3.9.1) — a {@code head}
     * (identifier / root / literal, with an optional dotted chain)
     * followed by {@code ' *'}, optional digits, and nothing after — or
     * -1 when the LHS is not a compact-tuple head. A reversed-dispatch
     * LHS never qualifies (§3.9 excludes it), so its trailing dot is
     * checked first. Detecting the marker lets {@link LnOnlyPhi} keep the
     * φ {@link Openness#OPEN} and route deeper-indent lines into the
     * star's {@code Φ.tuple} instead of {@link #bare(Tokens)} reading the
     * {@code *} as a completed empty-tuple argument (R-3.10.6). Consumes
     * the token stream.
     * @param tokens Token reader positioned at the LHS head
     * @return Index of the marker's leading space, or -1
     */
    private static int compactMarker(final Tokens tokens) {
        final int result;
        if (LnOnlyPhi.reversedAhead(tokens, tokens.readValue())) {
            result = -1;
        } else {
            tokens.readChain();
            result = LnOnlyPhi.trailingStar(tokens);
        }
        return result;
    }

    /**
     * Whether the cursor sits at a trailing compact-tuple marker — a
     * {@code ' *'} followed only by optional digits to the end of the
     * body — and, if so, the index of its leading space.
     * @param tokens Token reader positioned after the head and chain
     * @return Index of the marker's leading space, or -1
     */
    private static int trailingStar(final Tokens tokens) {
        int result = -1;
        final int space = tokens.cursor();
        if (!tokens.atEnd() && tokens.current() == ' ') {
            tokens.seek(space + 1);
            if (!tokens.atEnd() && tokens.current() == '*') {
                tokens.seek(tokens.cursor() + 1);
                while (!tokens.atEnd()
                    && tokens.current() >= '0' && tokens.current() <= '9') {
                    tokens.seek(tokens.cursor() + 1);
                }
                if (tokens.atEnd()) {
                    result = space;
                }
            }
        }
        return result;
    }

    /**
     * Read the {@code N} of a trailing compact-tuple marker {@code *N}
     * at {@code marker} in {@code lhs} — the digits after {@code ' *'},
     * defaulting to 0 when absent (R-3.9.1).
     * @param lhs The LHS body
     * @param marker Index of the marker's leading space
     * @return The N count
     */
    private static int starCount(final String lhs, final int marker) {
        int count = 0;
        int idx = marker + 2;
        while (idx < lhs.length()
            && lhs.charAt(idx) >= '0' && lhs.charAt(idx) <= '9') {
            count = count * 10 + lhs.charAt(idx) - '0';
            idx = idx + 1;
        }
        return count;
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
