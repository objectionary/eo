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
 * back to a {@link Emit#savepoint()}, the error is emitted, and the
 * block nested under the failed line is skipped up to the point
 * {@link Recovery} names.</p>
 *
 * <p>This is the analogue of {@code EoSyntax} on the ANTLR side, but
 * with no parser-grammar dependency: classification, validation, and
 * emission all happen here against the spec rules directly.</p>
 *
 * @since 0.1
 */
final class Eo implements Iterable<Directive> {

    /**
     * NAME-like terminator characters per §2.3 (excluding the dot,
     * which callers check explicitly).
     */
    private static final String NAME_TERMINATORS = " \t,|':;!?[]{}()";

    /**
     * Head characters of §3.1 that open a root-headed line without
     * opening a literal — group, star, root, and identity tokens.
     */
    private static final String ROOT_TOKENS = "*(QTI@^$";

    /**
     * Initial capacity of the source line buffer, {@link java.util.ArrayList}'s own default.
     */
    private static final int SPANS_CAPACITY = 10;

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
            (level, naming) -> Eo.checkOnClose(level, emit, naming),
            parent -> Eo.beforeChild(parent, emit)
        );
        final java.util.List<Span> spans = new java.util.ArrayList<>(Eo.SPANS_CAPACITY);
        new Source(this.source).forEach(spans::add);
        final Recovery recovery = new Recovery(spans);
        int idx = 0;
        while (idx < spans.size()) {
            final Span span = spans.get(idx);
            if (!globals.inTextBlock() && !span.trailing()
                && Eo.isBytesContinuation(span.body())) {
                idx = Eo.mergeBytesContinuation(spans, idx, stack, globals, emit, recovery);
            } else if (Eo.process(span, stack, globals, emit)) {
                idx = recovery.after(idx);
            } else {
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
        return Eo.topLevelMarker(body, idx -> Eo.spacedMarker(body, idx, "++>"));
    }

    /**
     * The index of the {@code -->} throwing-test shorthand marker used
     * in the inline-phi suffix position ({@code lhs --> name}, the
     * throwing counterpart of {@code ++>}), at paren depth 0 and
     * outside strings, or -1 if none exists. The marker must be
     * preceded by a space, which distinguishes the suffix use from the
     * head-of-line form ({@code --> name}) that {@link Eo#classify}
     * routes to a bare formation. Shared by the {@link Eo#onlyPhi}
     * classifier and {@link LnOnlyPhi}, so both agree on where a
     * compact throwing-test shorthand splits its LHS.
     * @param body Line body to scan
     * @return Index of the top-level marker, or -1
     */
    static int topLevelMinusMinusArrowIndex(final String body) {
        return Eo.topLevelMarker(body, idx -> Eo.spacedMarker(body, idx, "-->"));
    }

    private static boolean spacedMarker(
        final String body, final int idx, final String marker
    ) {
        return idx > 0 && body.charAt(idx - 1) == ' '
            && body.startsWith(marker, idx);
    }

    private static int topLevelMarker(
        final String body, final java.util.function.IntPredicate marker
    ) {
        int depth = 0;
        int found = -1;
        int idx = 0;
        while (idx < body.length() - 2 && found < 0) {
            final char glyph = body.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(body, idx);
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

    private static int mergeBytesContinuation(
        final java.util.List<Span> spans, final int start, final Stack stack,
        final Globals globals, final Emit emit, final Recovery recovery
    ) {
        final Span head = spans.get(start);
        final StringBuilder body = new StringBuilder(head.body().stripTrailing());
        int idx = start + 1;
        boolean broken = false;
        while (idx < spans.size()) {
            final Span next = spans.get(idx);
            final String trimmed = next.body().stripTrailing();
            if (!next.blank() && next.indent() < head.indent()) {
                emit.error(
                    next.line(), 0, "multi-line bytes continuation must not de-indent"
                );
                broken = true;
                break;
            }
            if (!Eo.isBytesOnly(trimmed)) {
                emit.error(
                    next.line(), 0, "multi-line bytes interrupted by non-byte content"
                );
                broken = true;
                break;
            }
            body.append(trimmed);
            idx = idx + 1;
            if (!Eo.isBytesContinuation(trimmed)) {
                break;
            }
        }
        final int resumption;
        if (broken) {
            resumption = recovery.skip(idx, head.indent());
        } else if (Eo.process(
            new Span(" ".repeat(head.indent()).concat(body.toString()), head.line()),
            stack, globals, emit
        )) {
            resumption = recovery.skip(idx, head.indent());
        } else {
            resumption = idx;
        }
        return resumption;
    }

    private static boolean isBytesContinuation(final String body) {
        final String trimmed = body.stripTrailing();
        return trimmed.length() >= 6 && trimmed.endsWith("-") && Eo.isBytesOnly(trimmed);
    }

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

    private static boolean hex(final char glyph) {
        return glyph >= '0' && glyph <= '9' || glyph >= 'A' && glyph <= 'F';
    }

    private static boolean process(
        final Span span, final Stack stack, final Globals globals, final Emit emit
    ) {
        boolean failed = false;
        if (globals.inTextBlock()) {
            Eo.continueTextBlock(span, stack, globals, emit);
        } else if (span.tab() && !span.blank()) {
            emit.error(span.line(), 0, "tab character in leading whitespace");
            failed = true;
        } else if (!span.blank() && span.indent() % 2 == 1) {
            emit.error(span.line(), 0, "unexpected odd indent");
            failed = true;
        } else if (span.trailing()) {
            emit.error(span.line(), 0, "trailing whitespace at end of line");
            failed = true;
        } else if (Eo.opensTextBlock(span)) {
            globals.openTextBlock(span.line(), span.indent());
            globals.markEmitted();
            globals.clearBlanks();
        } else {
            failed = Eo.dispatch(span, stack, globals, emit);
        }
        return failed;
    }

    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    private static void continueTextBlock(
        final Span span, final Stack stack, final Globals globals, final Emit emit
    ) {
        if (Eo.closesTextBlock(span, globals)) {
            stack.popDeeperThan(span.indent());
            final Savepoint token = emit.savepoint();
            final java.util.List<Level> frame = stack.snapshot();
            try {
                new LnTextBlock(span).into(stack, globals, emit);
            } catch (final ParseError err) {
                emit.rollback(token);
                stack.restore(frame);
                emit.error(err.line(), err.pos(), err.getMessage());
                globals.closeTextBlock();
            }
        } else {
            final String raw = span.text();
            if (span.trailing()) {
                emit.error(span.line(), 0, "trailing whitespace at end of line");
            }
            if (!Eo.isBlank(raw) && Eo.leadingSpaces(raw) < globals.textBlockOpenIndent()) {
                emit.error(
                    span.line(), 0,
                    "text block body line indented less than opener"
                );
            }
            globals.appendTextLine(raw);
        }
    }

    private static int leadingSpaces(final String raw) {
        int count = 0;
        while (count < raw.length() && raw.charAt(count) == ' ') {
            count = count + 1;
        }
        return count;
    }

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

    private static boolean dispatch(
        final Span span, final Stack stack, final Globals globals, final Emit emit
    ) {
        if (!span.blank() && span.head() != '#') {
            stack.popDeeperThan(span.indent());
        }
        final Savepoint token = emit.savepoint();
        final java.util.List<Level> frame = stack.snapshot();
        final Globals saved = globals.savepoint();
        boolean failed = false;
        try {
            Eo.classify(span).into(stack, globals, emit);
        } catch (final ParseError err) {
            emit.rollback(token);
            stack.restore(frame);
            globals.restore(saved);
            emit.error(err.line(), err.pos(), err.getMessage(), true);
            failed = true;
        }
        return failed;
    }

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

    private static boolean closesTextBlock(final Span span, final Globals globals) {
        final String body = span.body();
        return !span.blank()
            && span.indent() == globals.textBlockOpenIndent()
            && body.startsWith("\"\"\"")
            && " .:".indexOf(body.concat(" ").charAt(3)) >= 0;
    }

    private static Line classify(final Span span) {
        final Line line;
        if (span.blank()) {
            line = new LnBlank(span);
        } else if (span.head() == '#') {
            line = new LnComment(span);
        } else if (Eo.metaHead(span)) {
            line = Eo.plussed(span);
        } else if (Eo.throwHead(span)) {
            line = new LnFormation(span);
        } else if (span.head() == '[') {
            line = new LnFormation(span);
        } else if (span.head() == '.') {
            line = new LnMethod(span);
        } else if (span.head() == '|') {
            line = new LnPipe(span);
        } else if (span.head() == '?') {
            line = Eo.questioned(span);
        } else {
            line = Eo.applied(span);
        }
        return line;
    }

    private static Line applied(final Span span) {
        final Line line;
        if (Eo.nameHead(span)) {
            line = Eo.applicative(span, Eo.reversedDispatch(span));
        } else if (Eo.rootHead(span)) {
            line = Eo.applicative(span, Eo.rootReversedDispatch(span));
        } else {
            line = Eo.rejected(span);
        }
        return line;
    }

    private static Line rejected(final Span span) {
        final String reason;
        if (span.body().codePoints().findFirst().orElse(0) == 0x1F335) {
            reason = "cactus emoji is reserved for auto-names; not allowed as a line head";
        } else {
            reason = "line shape not yet implemented in spec parser";
        }
        return (stack, globals, emit) -> {
            throw new ParseError(span.line(), span.indent(), reason);
        };
    }

    private static boolean metaHead(final Span span) {
        return span.head() == '+' && !Eo.signedDigit(span);
    }

    private static boolean throwHead(final Span span) {
        return span.head() == '-' && span.body().startsWith("-->");
    }

    private static boolean nameHead(final Span span) {
        return span.head() >= 'a' && span.head() <= 'z';
    }

    private static Line plussed(final Span span) {
        final Line line;
        if (span.body().startsWith("++>")) {
            line = new LnFormation(span);
        } else {
            line = new LnMeta(span);
        }
        return line;
    }

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

    private static boolean rootHead(final Span span) {
        return Eo.tokenHead(span.head()) || Eo.literalHead(span);
    }

    private static boolean tokenHead(final char head) {
        return Eo.ROOT_TOKENS.indexOf(head) >= 0;
    }

    private static boolean literalHead(final Span span) {
        final char head = span.head();
        return head == '"' || Eo.bytesHead(head) || Eo.numberHead(span);
    }

    private static boolean bytesHead(final char head) {
        return head >= 'A' && head <= 'F' || head == '-';
    }

    private static boolean numberHead(final Span span) {
        final char head = span.head();
        return head >= '0' && head <= '9' || Eo.signedDigit(span);
    }

    private static boolean signedDigit(final Span span) {
        final String body = span.body();
        return body.length() >= 2
            && body.charAt(1) >= '0' && body.charAt(1) <= '9';
    }

    private static Line questioned(final Span span) {
        final Line line;
        if (Eo.qdot(span.body(), 0)) {
            line = new LnMethod(span);
        } else {
            line = new LnVoid(span);
        }
        return line;
    }

    private static boolean qdot(final String body, final int idx) {
        return idx + 1 < body.length()
            && body.charAt(idx) == '?' && body.charAt(idx + 1) == '.';
    }

    private static boolean onlyPhi(final Span span) {
        return Eo.topLevelGreaterBracketIndex(span.body()) >= 0
            || Eo.topLevelPlusPlusArrowIndex(span.body()) >= 0
            || Eo.topLevelMinusMinusArrowIndex(span.body()) >= 0;
    }

    private static boolean compactTuple(final Span span) {
        final String body = span.body();
        int depth = 0;
        int idx = 0;
        while (idx < body.length() && (depth > 0 || body.charAt(idx) != ' ')) {
            final char glyph = body.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(body, idx);
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            }
            idx = idx + 1;
        }
        boolean compact = false;
        if (idx + 1 < body.length() && body.charAt(idx + 1) == '*') {
            final int after = idx + 2;
            compact = after >= body.length() || Eo.starTail(body, after);
        }
        return compact;
    }

    private static boolean starTail(final String body, final int after) {
        final char next = body.charAt(after);
        final boolean confirmed;
        if (next == ' ') {
            confirmed = Eo.suffixAt(body, after + 1);
        } else {
            confirmed = next == '>' || next >= '0' && next <= '9';
        }
        return confirmed;
    }

    private static boolean suffixAt(final String body, final int from) {
        final boolean starts;
        if (from >= body.length()) {
            starts = false;
        } else if (body.charAt(from) == '>') {
            starts = true;
        } else {
            starts = from + 1 < body.length()
                && (body.charAt(from) == '+' || body.charAt(from) == '-')
                && body.charAt(from + 1) == '>';
        }
        return starts;
    }

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

    private static boolean nameTerminator(final char glyph) {
        return Eo.NAME_TERMINATORS.indexOf(glyph) >= 0;
    }

    private static void checkOnClose(final Level level, final Emit emit, final boolean naming) {
        try {
            level.commitArg();
        } catch (final ParseError err) {
            emit.error(err.line(), err.pos(), err.getMessage());
        }
        Eo.checkNaming(level, emit, naming);
        if (level.kind() == Kind.BARE_REVERSED && !level.taken()) {
            emit.error(
                level.start(), level.indent(),
                "reversed dispatch missing receiver"
            );
        }
        if (level.kind() == Kind.COMPACT_TUPLE || level.star()) {
            Eo.closeCompactTuple(level, emit);
        }
        if (level.kind() == Kind.ONLY_PHI
            && level.openness() != Openness.HCOMPLETED) {
            emit.close();
        }
        emit.close();
    }

    private static void checkNaming(final Level level, final Emit emit, final boolean naming) {
        if (naming && !level.named()
            && (level.parent() == Kind.TOP_LEVEL
                || level.parent() == Kind.BARE_FORMATION)) {
            emit.error(
                level.start(), level.indent(),
                "object inside formation must have a name"
            );
        }
        if (naming && level.argument() && level.named()) {
            emit.error(
                level.start(), level.indent(),
                level.onlyPhiNamingError()
            );
        }
    }

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
            emit.unnamedObject(
                "Φ.tuple", level.start(), level.indent()
            );
            emit.star();
            emit.close();
        } else if (level.tupled()) {
            emit.close();
        }
    }

    private static void beforeChild(final Level parent, final Emit emit) {
        if ((parent.kind() == Kind.COMPACT_TUPLE || parent.star())
            && !parent.tupled()
            && parent.children() == parent.count()) {
            emit.unnamedObject(
                "Φ.tuple", parent.start(), parent.indent()
            );
            emit.star();
            parent.openTuple();
        }
        parent.child();
    }
}
