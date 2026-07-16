/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import org.xembly.Directive;

/**
 * An EO program — the top-level entry of the spec-driven parser.
 *
 * <p>Wraps EO source text and exposes the parsed XMIR as an iterable of
 * Xembly {@link Directive}s. The walk is single-pass: each {@link Span}
 * from {@link Source} is classified into a {@link Line} per Appendix B
 * of the spec, then dispatched into the shared {@link Stack},
 * {@link Globals}, and {@link Emit} state. Recovery (§7) is line-local
 * — a {@link ParseError} is caught, the line's directives are rolled
 * back to a {@link Emit#savepoint()}, and the error is emitted.</p>
 *
 * <p>This is the analogue of {@code EoSyntax} on the ANTLR side, but
 * with no parser-grammar dependency: classification, validation, and
 * emission all happen here against the spec rules directly.</p>
 *
 * <p>The classifier in this initial implementation covers the three
 * trivial line shapes — blank, comment, meta. Other shapes from §3.1
 * fall through to a placeholder error pending later additions.</p>
 *
 * @since 0.1
 * @checkstyle CyclomaticComplexityCheck (820 lines)
 * @checkstyle BooleanExpressionComplexityCheck (820 lines)
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnnecessaryLocalRule", "PMD.CognitiveComplexity"})
final class Eo implements Iterable<Directive> {

    /**
     * NAME-like terminator characters per §2.3 (excluding the dot,
     * which callers check explicitly).
     */
    private static final String NAME_TERMINATORS = " \t,|':;!?[]{}()";

    /**
     * Raw EO source text.
     */
    private final String source;

    /**
     * Ctor.
     * @param text The EO program text
     */
    Eo(final String text) {
        this.source = text;
    }

    @Override
    public Iterator<Directive> iterator() {
        return this.directives().iterator();
    }

    /**
     * Walk the source and accumulate directives.
     * @return Directives in source order
     */
    Iterable<Directive> directives() {
        final Globals globals = new Globals();
        final Emit emit = new Emit(this.source);
        final Stack stack = new Stack(
            level -> Eo.checkOnClose(level, emit),
            parent -> Eo.beforeChild(parent, emit)
        );
        final java.util.List<Span> spans = new java.util.ArrayList<>(16);
        new Source(this.source).forEach(spans::add);
        int idx = 0;
        while (idx < spans.size()) {
            final Span span = spans.get(idx);
            if (!globals.inTextBlock() && Eo.isBytesContinuation(span.body())) {
                final int next = Eo.mergeBytesContinuation(spans, idx, stack, globals, emit);
                idx = next;
            } else {
                Eo.process(span, stack, globals, emit);
                idx = idx + 1;
            }
        }
        stack.close();
        Eo.finish(globals, emit);
        return emit.directives();
    }

    /**
     * The index of the substring {@code "> ["} at paren depth 0 (i.e.,
     * outside of any paren group and outside of strings), or -1 if
     * none exists. Shared by the {@link Eo#onlyPhi} classifier and
     * {@link LnOnlyPhi}, so both agree on where an only-phi formation
     * splits.
     * @param body Line body to scan
     * @return Index of the top-level marker, or -1
     */
    static int topLevelGreaterBracketIndex(final String body) {
        return Eo.topLevelMarker(
            body,
            idx -> body.charAt(idx) == '>'
                && body.charAt(idx + 1) == ' '
                && body.charAt(idx + 2) == '['
        );
    }

    /**
     * The index of the {@code ++>} test shorthand marker used in the
     * inline-phi suffix position ({@code lhs ++> name}, R-3.10.8 /
     * R-6.3.6), at paren depth 0 and outside strings, or -1 if none
     * exists. The marker must be preceded by a space, which
     * distinguishes the suffix use from the head-of-line form
     * ({@code ++> name}) that {@link Eo#plussed} routes to a bare
     * formation. Shared by the {@link Eo#onlyPhi} classifier and
     * {@link LnOnlyPhi}, so both agree on where a compact test
     * shorthand splits its LHS.
     * @param body Line body to scan
     * @return Index of the top-level marker, or -1
     */
    static int topLevelPlusPlusArrowIndex(final String body) {
        return Eo.topLevelMarker(
            body,
            idx -> idx > 0 && body.charAt(idx) == '+'
                && body.charAt(idx - 1) == ' '
                && body.charAt(idx + 1) == '+'
                && body.charAt(idx + 2) == '>'
        );
    }

    /**
     * The index of the first position where {@code marker} matches, scanned
     * at paren depth 0 and outside string literals. Both
     * {@link Eo#topLevelGreaterBracketIndex} and
     * {@link Eo#topLevelPlusPlusArrowIndex} drive their three-character
     * lookahead through this shared skeleton, so the paren/string handling
     * lives in one place. The scan stops two characters short of the end, so
     * a marker predicate may safely read {@code idx + 1} and {@code idx + 2}.
     * @param body Line body to scan
     * @param marker Predicate testing whether the marker starts at an index
     * @return Index of the first top-level match, or -1
     */
    private static int topLevelMarker(
        final String body, final java.util.function.IntPredicate marker
    ) {
        int depth = 0;
        boolean instr = false;
        int found = -1;
        int idx = 0;
        while (idx < body.length() - 2 && found < 0) {
            final char glyph = body.charAt(idx);
            if (instr) {
                if (glyph == '\\' && idx + 1 < body.length()) {
                    idx = idx + 2;
                    continue;
                }
                if (glyph == '"') {
                    instr = false;
                }
            } else if (glyph == '"') {
                instr = true;
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            } else if (depth == 0 && marker.test(idx)) {
                found = idx;
            }
            idx = idx + 1;
        }
        return found;
    }

    /**
     * Merge a BYTES continuation starting at index {@code start} —
     * concatenates the trailing-dash chunk with subsequent BYTES-only
     * lines at &gt;= the same indent until a chunk without trailing
     * dash terminates the tstartsen (R-3.13).
     * @param spans Materialised list of source spans
     * @param start Index of the continuation start
     * @param stack Indent stack
     * @param globals Global parser state
     * @param emit Directives sink
     * @return Next index to process (past the merged region)
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static int mergeBytesContinuation(
        final java.util.List<Span> spans, final int start,
        final Stack stack, final Globals globals, final Emit emit
    ) {
        final Span head = spans.get(start);
        final StringBuilder body = new StringBuilder(head.body());
        int idx = start + 1;
        while (idx < spans.size()) {
            final Span next = spans.get(idx);
            if (next.indent() < head.indent() || !Eo.isBytesOnly(next.body())) {
                break;
            }
            body.append(next.body());
            idx = idx + 1;
            if (!next.body().endsWith("-")) {
                break;
            }
        }
        final Span merged = new Span(
            " ".repeat(head.indent()).concat(body.toString()), head.line()
        );
        Eo.process(merged, stack, globals, emit);
        return idx;
    }

    /**
     * Whether a span body is the start of a multi-line BYTES literal —
     * purely bytes-only content, length &gt;= 6, ending with {@code -}.
     * Per R-3.13.1, single-byte form ({@code BB-}) never continues, so
     * we require &gt;=2 bytes (≥ 6 chars).
     * @param body The line body
     * @return True if a BYTES continuation starts here
     */
    private static boolean isBytesContinuation(final String body) {
        return body.length() >= 6
            && body.endsWith("-")
            && Eo.isBytesOnly(body);
    }

    /**
     * Whether the body is purely a sequence of hex bytes with dash
     * separators — {@code HH-HH} or {@code HH-HH-} etc.
     * @param body The line body
     * @return True if the body matches the bytes-only pattern
     */
    private static boolean isBytesOnly(final String body) {
        boolean valid = !body.isEmpty();
        int idx = 0;
        while (valid && idx < body.length()) {
            if (idx + 1 >= body.length()
                || !Eo.hex(body.charAt(idx))
                || !Eo.hex(body.charAt(idx + 1))) {
                valid = false;
            } else {
                idx = idx + 2;
                if (idx < body.length() && body.charAt(idx) != '-') {
                    valid = false;
                } else {
                    idx = idx + 1;
                }
            }
        }
        return valid;
    }

    /**
     * Whether a character is a valid BYTES hex digit. Per the grammar
     * (matching ANTLR's {@code BYTE : [0-9A-F][0-9A-F]}), BYTES accept
     * only uppercase hex — lowercase letters belong to {@code NAME}
     * tstartsens. Compare with the case-insensitive
     * {@link Tstartsens#hexDigit(char)} used for the {@code 0x...} HEX
     * literal (R-9.8.3).
     * @param glyph The character
     * @return True if 0-9 or A-F
     */
    private static boolean hex(final char glyph) {
        return glyph >= '0' && glyph <= '9' || glyph >= 'A' && glyph <= 'F';
    }

    /**
     * Process one source line.
     *
     * <p>Classifies the span into a {@link Line} per Appendix B, takes a
     * savepoint on {@code emit}, dispatches the line, and on
     * {@link ParseError} rolls back and emits the error. Step A pops on
     * indent transitions run before classification when the stack is
     * non-empty.</p>
     *
     * @param span The source span
     * @param stack The indent stack
     * @param globals The global parser state
     * @param emit The directives sink
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static void process(
        final Span span, final Stack stack, final Globals globals, final Emit emit
    ) {
        if (globals.inTextBlock()) {
            Eo.continueTextBlock(span, stack, globals, emit);
        } else if (span.tab()) {
            emit.error(span.line(), 0, "tab character in leading whitespace");
        } else if (!span.blank() && span.indent() % 2 == 1) {
            emit.error(span.line(), 0, "unexpected odd indent");
        } else if (Eo.opensTextBlock(span)) {
            globals.openTextBlock(span.line(), span.indent());
            globals.markEmitted();
            globals.clearBlanks();
        } else {
            Eo.dispatch(span, stack, globals, emit);
        }
    }

    /**
     * Handle a line that arrives while inside an open text block —
     * either close the block or append the line to the body.
     * @param span The source span
     * @param stack The indent stack
     * @param globals The global parser state
     * @param emit The directives sink
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static void continueTextBlock(
        final Span span, final Stack stack, final Globals globals, final Emit emit
    ) {
        if (Eo.closesTextBlock(span, globals)) {
            final int tstartsen = emit.savepoint();
            try {
                stack.popDeeperThan(span.indent());
                new LnTextBlock(span).into(stack, globals, emit);
            } catch (final ParseError err) {
                emit.rollback(tstartsen);
                emit.error(err.line(), err.pos(), err.getMessage());
            }
        } else {
            final String raw = span.text();
            if (!Eo.isBlank(raw) && Eo.leadingSpaces(raw) < globals.textBlockOpenIndent()) {
                emit.error(
                    span.line(), 0,
                    "text block body line indented less than opener"
                );
            }
            globals.appendTextLine(raw);
        }
    }

    /**
     * Count the leading space characters on a raw text line.
     * @param raw The line text
     * @return Number of leading spaces
     */
    private static int leadingSpaces(final String raw) {
        int count = 0;
        while (count < raw.length() && raw.charAt(count) == ' ') {
            count = count + 1;
        }
        return count;
    }

    /**
     * Whether a string is empty or contains only whitespace.
     * @param raw The string
     * @return True when blank
     */
    private static boolean isBlank(final String raw) {
        boolean blank = true;
        for (int idx = 0; idx < raw.length(); idx = idx + 1) {
            if (!Character.isWhitespace(raw.charAt(idx))) {
                blank = false;
                break;
            }
        }
        return blank;
    }

    /**
     * Classify the line and dispatch into its {@link Line} impl with
     * the per-line savepoint applied for recovery.
     * @param span The source span
     * @param stack The indent stack
     * @param globals The global parser state
     * @param emit The directives sink
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static void dispatch(
        final Span span, final Stack stack, final Globals globals, final Emit emit
    ) {
        if (!span.blank() && span.head() != '#') {
            stack.popDeeperThan(span.indent());
        }
        final int tstartsen = emit.savepoint();
        final int frame = stack.size();
        try {
            Eo.classify(span).into(stack, globals, emit);
        } catch (final ParseError err) {
            emit.rollback(tstartsen);
            stack.silentTruncate(frame);
            emit.error(err.line(), err.pos(), err.getMessage());
        }
    }

    /**
     * Whether the span is a text-block opener — body is exactly
     * {@code """} (possibly with trailing whitespace).
     * @param span The span
     * @return True if the line opens a text block
     */
    private static boolean opensTextBlock(final Span span) {
        boolean opens = !span.blank() && span.body().startsWith("\"\"\"");
        if (opens) {
            final String body = span.body();
            for (int idx = 3; idx < body.length(); idx = idx + 1) {
                if (body.charAt(idx) != ' ' && body.charAt(idx) != '\t') {
                    opens = false;
                    break;
                }
            }
        }
        return opens;
    }

    /**
     * Whether the span closes the current open text block. Per R-3.11.3
     * the closer must be a {@code """} line at the opener's indent;
     * deeper-indent {@code """} are body content.
     * @param span The span
     * @param globals The global parser state
     * @return True if the line closes the current text block
     */
    private static boolean closesTextBlock(final Span span, final Globals globals) {
        return !span.blank()
            && span.indent() == globals.textBlockOpenIndent()
            && span.body().startsWith("\"\"\"");
    }

    /**
     * Classify a span into a {@link Line} per Appendix B.
     * @param span The span
     * @return The classified line
     * @checkstyle NPathComplexityCheck (60 lines)
     */
    private static Line classify(final Span span) {
        final Line line;
        if (span.blank()) {
            line = new LnBlank(span);
        } else if (span.head() == '#') {
            line = new LnComment(span);
        } else if (span.head() == '+' && !Eo.signedDigit(span)) {
            line = Eo.plussed(span);
        } else if (span.head() == '[') {
            line = new LnFormation(span);
        } else if (span.head() == '.') {
            line = new LnMethod(span);
        } else if (span.head() == '|') {
            line = new LnPipe(span);
        } else if (span.head() == '?') {
            line = Eo.questioned(span);
        } else if (span.head() >= 'a' && span.head() <= 'z') {
            line = Eo.applicative(span, Eo.reversedDispatch(span));
        } else if (span.head() == '*'
            || span.head() == '"'
            || span.head() == '('
            || span.head() == 'Q'
            || span.head() == 'T'
            || span.head() == '@'
            || span.head() == '^'
            || span.head() == '$'
            || span.head() == '%'
            || span.head() >= '0' && span.head() <= '9'
            || span.head() >= 'A' && span.head() <= 'F'
            || span.head() == '-'
            || Eo.signedDigit(span)) {
            line = Eo.applicative(span, Eo.rootReversedDispatch(span));
        } else if (span.body().codePoints().findFirst().orElse(0) == 0x1F335) {
            line = (stack, globals, emit) -> {
                throw new ParseError(
                    span.line(), span.indent(),
                    "cactus emoji is reserved for auto-names; not allowed as a line head"
                );
            };
        } else {
            line = (stack, globals, emit) -> {
                throw new ParseError(
                    span.line(), span.indent(),
                    "line shape not yet implemented in spec parser"
                );
            };
        }
        return line;
    }

    /**
     * Classify a {@code +}-headed non-number line (§3.1): the
     * {@code ++>} test-attribute shorthand desugars to a bare
     * formation with a {@code +>} suffix (R-6.3.6); anything else is
     * a meta directive (§3.2).
     * @param span The line span
     * @return The line shape
     */
    private static Line plussed(final Span span) {
        final Line line;
        if (span.body().startsWith("++>")) {
            line = new LnFormation(span);
        } else {
            line = new LnMeta(span);
        }
        return line;
    }

    /**
     * Classify an application-like line — the inner dispatch shared by
     * the identifier-headed and root-headed line groups (§3.1):
     * reversed dispatch, only-phi formation, compact tuple, or plain
     * application.
     *
     * <p>An only-phi formation ({@code lhs > [voids] > name}) is
     * recognised before a bare reversed dispatch, so a trailing-dot LHS
     * carrying an inline void suffix ({@code if. > [t] >> rec}) is
     * routed to {@link LnOnlyPhi} — which understands the {@code [voids]}
     * suffix — rather than to {@link LnReversed}, which would hand the
     * whole {@code > [t] >> rec} tail to the void-unaware suffix parser
     * and reject the leading {@code [} as trailing garbage.</p>
     *
     * @param span The line span
     * @param reversed Whether the line is a reversed dispatch
     * @return The line shape
     */
    private static Line applicative(final Span span, final boolean reversed) {
        final Line line;
        if (Eo.onlyPhi(span)) {
            line = new LnOnlyPhi(span);
        } else if (reversed) {
            line = new LnReversed(span);
        } else if (Eo.compactTuple(span)) {
            line = new LnCompactTuple(span);
        } else {
            line = new LnApplication(span);
        }
        return line;
    }

    /**
     * End-of-stream checks — §8 of the spec.
     *
     * <p>Reports unclosed text blocks (R-8.2) and excessive trailing
     * blanks (R-8.4), and flushes a top comment block left pending in a
     * comment-only file (R-8.3). EOF stack popping with close-time checks
     * already ran via {@link Stack#close(Stack.Closer)}.</p>
     *
     * @param globals The global parser state
     * @param emit The directives sink
     */
    private static void finish(final Globals globals, final Emit emit) {
        if (globals.inTextBlock()) {
            emit.error(
                globals.textBlockOpenLine(), 0,
                String.format(
                    "unclosed text block opened at line %d",
                    globals.textBlockOpenLine()
                )
            );
        }
        if (!globals.pendingComments().isEmpty()) {
            final java.util.List<Span> pending = globals.pendingComments();
            emit.comment(pending, pending.get(pending.size() - 1).line());
            globals.clearComments();
        }
        if (globals.trailingBlanks() > 1) {
            emit.error(0, 0, "more than one trailing blank line");
        }
    }

    /**
     * Determine whether a {@code +}-headed span is the start of a signed
     * number (per R-3.2.5) rather than a meta directive.
     * @param span The span (its head is {@code +})
     * @return True if the character after {@code +} is a digit
     */
    private static boolean signedDigit(final Span span) {
        final String body = span.body();
        return body.length() >= 2
            && body.charAt(1) >= '0' && body.charAt(1) <= '9';
    }

    /**
     * Classify a {@code ?}-headed line: a fragile method-continuation
     * line ({@code ?.method}) when a {@code .} immediately follows the
     * {@code ?} (R-3.5), otherwise a vertical void ({@code ? > name},
     * §3.4).
     * @param span The span (its head is {@code ?})
     * @return The line shape
     */
    private static Line questioned(final Span span) {
        final Line line;
        if (Eo.qdot(span.body(), 0)) {
            line = new LnMethod(span);
        } else {
            line = new LnVoid(span);
        }
        return line;
    }

    /**
     * Whether the fragile {@code ?.} operator appears at {@code idx} in
     * {@code body} — a {@code ?} immediately followed by a {@code .}.
     * @param body The line body
     * @param idx Index to test
     * @return True if {@code ?.} starts at {@code idx}
     */
    private static boolean qdot(final String body, final int idx) {
        return idx + 1 < body.length()
            && body.charAt(idx) == '?' && body.charAt(idx + 1) == '.';
    }

    /**
     * Whether an identifier-headed span carries an only-phi
     * formation suffix — either the explicit {@code > [params] > name}
     * form (detected by a {@code "> ["} search) or the compact
     * {@code ++> name} test shorthand ({@code lhs ++> name}, R-3.10.8),
     * which desugars to {@code lhs > [] +> name}. The authoritative
     * parse is in {@link LnOnlyPhi}.
     * @param span The span
     * @return True if the line shape is only-phi
     */
    private static boolean onlyPhi(final Span span) {
        return Eo.topLevelGreaterBracketIndex(span.body()) >= 0
            || Eo.topLevelPlusPlusArrowIndex(span.body()) >= 0;
    }

    /**
     * Whether an identifier-headed span is a compact-tuple line —
     * i.e., contains {@code ' *'} followed by digits, end-of-line, or
     * a suffix marker (Appendix B + §3.9). The scan is approximate;
     * the {@link LnCompactTuple} parser does the authoritative check.
     * @param span The span (its head is a lowercase letter)
     * @return True if the line shape is compact-tuple
     */
    private static boolean compactTuple(final Span span) {
        final String body = span.body();
        int idx = 0;
        while (idx < body.length() && body.charAt(idx) != ' ') {
            idx = idx + 1;
        }
        boolean compact = false;
        if (idx + 1 < body.length() && body.charAt(idx + 1) == '*') {
            final int after = idx + 2;
            if (after >= body.length()) {
                compact = true;
            } else {
                final char next = body.charAt(after);
                compact = next >= '0' && next <= '9' || next == '>'
                    || next == ' ' && Eo.suffixAt(body, after + 1);
            }
        }
        return compact;
    }

    /**
     * Whether the body starting at {@code from} begins with a suffix
     * marker (R-3.10) — {@code >}, {@code >>}, or {@code +>}.
     * @param body Line body
     * @param from Index to inspect
     * @return True if a suffix marker starts here
     */
    private static boolean suffixAt(final String body, final int from) {
        final boolean starts;
        if (from >= body.length()) {
            starts = false;
        } else if (body.charAt(from) == '>') {
            starts = true;
        } else {
            starts = from + 1 < body.length()
                && body.charAt(from) == '+'
                && body.charAt(from + 1) == '>';
        }
        return starts;
    }

    /**
     * Determine whether an identifier-headed span is a reversed
     * dispatch — i.e., the leading NAME is immediately followed by
     * {@code .} and then a space or end-of-body (Appendix B).
     * @param span The span (its head is a lowercase letter)
     * @return True if the line shape is reversed dispatch
     */
    private static boolean reversedDispatch(final Span span) {
        final String body = span.body();
        int idx = 0;
        boolean reversed = false;
        while (idx < body.length()) {
            final char glyph = body.charAt(idx);
            if (glyph == '.') {
                reversed = idx + 1 >= body.length() || body.charAt(idx + 1) == ' ';
                break;
            }
            if (Eo.qdot(body, idx)) {
                reversed = idx + 2 >= body.length() || body.charAt(idx + 2) == ' ';
                break;
            }
            if (Eo.nameTerminator(glyph)) {
                break;
            }
            idx = idx + 1;
        }
        return reversed;
    }

    /**
     * Whether a root-headed line (e.g., {@code ^.} or {@code @.}) is
     * a reversed-dispatch — the root character is immediately
     * followed by {@code .} and then a space or end-of-body. The
     * head's source tstartsen maps to its XMIR symbol per R-9.3 inside
     * {@link LnReversed}.
     * @param span The span
     * @return True if the line shape is reversed dispatch with a
     *  root head
     */
    private static boolean rootReversedDispatch(final Span span) {
        final char head = span.head();
        final String body = span.body();
        final boolean root = head == '^' || head == '@' || head == '$';
        final boolean reversed;
        if (root && body.length() >= 2 && body.charAt(1) == '.') {
            reversed = body.length() == 2 || body.charAt(2) == ' ';
        } else if (root && Eo.qdot(body, 1)) {
            reversed = body.length() == 3 || body.charAt(3) == ' ';
        } else {
            reversed = false;
        }
        return reversed;
    }

    /**
     * Whether a character terminates a NAME-like identifier scan per
     * §2.3 (excluding the dot, since callers check it explicitly).
     * @param glyph Character
     * @return True if the character ends the name scan
     */
    private static boolean nameTerminator(final char glyph) {
        return Eo.NAME_TERMINATORS.indexOf(glyph) >= 0;
    }

    /**
     * Close-time check hostarts (§5.3).
     *
     * <p>Runs semantic checks on the popped level and balances the XML
     * cursor with an {@link Emit#close()} — every {@link Level} pushed
     * corresponds to one open {@code <o>} that must be closed when the
     * level pops. Two kinds open a second {@code <o>} and so emit an
     * extra close: a compact tuple (its {@code Φ.tuple} wrapper) and an
     * open only-phi formation (its φ application, left open for vertical
     * arguments per §4.5).</p>
     *
     * <p>Checks: R-5.3.1 (naming requirement for plain children of
     * formations and top-level objects), the only-phi argument-naming
     * ban (§4.5), bare-reversed receiver presence, and the compact-tuple
     * count. Atom-body and test-depth checks attach as their owning line
     * shapes are implemented.</p>
     *
     * @param level The level being closed
     * @param emit The directives sink
     */
    private static void checkOnClose(final Level level, final Emit emit) {
        try {
            level.commitArg(null);
        } catch (final ParseError err) {
            emit.error(err.line(), err.pos(), err.getMessage());
        }
        Eo.checkNaming(level, emit);
        if (level.kind() == Kind.BARE_REVERSED && !level.taken()) {
            emit.error(
                level.start(), level.indent(),
                "reversed dispatch missing receiver"
            );
        }
        if (level.kind() == Kind.COMPACT_TUPLE) {
            Eo.closeCompactTuple(level, emit);
        }
        if (level.kind() == Kind.ONLY_PHI_FORMATION
            && level.openness() != Openness.HORIZONTAL_COMPLETED) {
            emit.close();
        }
        emit.close();
    }

    /**
     * Report naming violations on the popped level: a plain child of a
     * formation or top-level object that lacks a name (R-5.3.1), and an
     * only-phi argument that carries one — the formation binds only φ,
     * so its φ's arguments may not be named (§4.5).
     * @param level The level being closed
     * @param emit The directives sink
     */
    private static void checkNaming(final Level level, final Emit emit) {
        if (!level.named()
            && (level.parent() == Kind.TOP_LEVEL
                || level.parent() == Kind.BARE_FORMATION)) {
            emit.error(
                level.start(), level.indent(),
                "object inside formation must have a name"
            );
        }
        if (level.argument() && level.named()) {
            emit.error(
                level.start(), level.indent(),
                "argument of an only-phi formation cannot carry a name suffix"
            );
        }
    }

    /**
     * Compact-tuple close: report under-count, then either synthesise
     * the empty {@code Φ.tuple star=""} wrapper (when {@code N} matched
     * the children exactly and nothing was emitted yet) or close the
     * already-open wrapper.
     * @param level The compact-tuple level
     * @param emit The directives sink
     */
    private static void closeCompactTuple(final Level level, final Emit emit) {
        if (level.children() < level.count()) {
            emit.error(
                level.start(), level.indent(),
                String.format(
                    "compact tuple requires at least %d children, got %d",
                    level.count(), level.children()
                )
            );
        }
        if (!level.tupled() && level.children() == level.count()) {
            emit.object(
                null, "Φ.tuple", level.start(), level.indent()
            );
            emit.star();
            emit.close();
        } else if (level.tupled()) {
            emit.close();
        }
    }

    /**
     * Pre-child hostarts (§3.9 / R-3.9.2): emit the synthesised
     * {@code Φ.tuple} wrapper exactly once when a compact-tuple parent
     * has accumulated its first N direct children.
     * @param parent The parent level
     * @param emit The directives sink
     */
    private static void beforeChild(final Level parent, final Emit emit) {
        if (parent.kind() == Kind.COMPACT_TUPLE
            && !parent.tupled()
            && parent.children() == parent.count()) {
            emit.object(
                null, "Φ.tuple", parent.start(), parent.indent()
            );
            emit.star();
            parent.openTuple();
        }
        parent.child();
    }
}
