/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.math.BigDecimal;
import java.util.List;

/**
 * Shared {@link Value}-to-XMIR rendering helpers.
 *
 * <p>Different line shapes ({@link LnApplication}, {@link LnMethod},
 * {@link LnReversed}, {@link LnCompactTuple}, {@link LnOnlyPhi},
 * …) all need to render parsed {@link Value}s and full expressions
 * into XMIR. This class centralises the recipes so every line emits
 * literals and chains in exactly the same way (§9.0.3 / §9.4 /
 * §9.4.2).</p>
 *
 * @since 0.1
 * @checkstyle CyclomaticComplexityCheck (600 lines)
 * @checkstyle BooleanExpressionComplexityCheck (600 lines)
 */
@SuppressWarnings({"PMD.UnnecessaryLocalRule", "PMD.TooManyMethods", "PMD.CognitiveComplexity"})
final class Emissions {

    /**
     * Maximum value of a {@code \NNN} octal byte escape (0o377, one byte).
     */
    private static final int MAX_OCTAL_BYTE = 0xFF;

    /**
     * No instances.
     */
    private Emissions() {
    }

    /**
     * Emit a full application expression read from {@code tokens} —
     * head, optional {@code .method} chain, and optional horizontal
     * args (§9.0.3). The outermost {@code <o>} (head or chain's last
     * link) is left <em>open</em> for the caller to close.
     * @param emit Emitter
     * @param name Name to attach to the outermost {@code <o>}, or
     *  {@code null}
     * @param tokens Token reader (cursor positioned at the head)
     * @param line Source line number
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void expression(
        final Emit emit, final String name, final Tokens tokens, final int line
    ) {
        final Value head = tokens.readValue();
        if (Emissions.reversedDispatch(tokens, head)) {
            tokens.seek(tokens.cursor() + 1);
            final List<Value> rargs = tokens.readArgs();
            emit.object(name, ".".concat(head.raw()), line, head.pos());
            for (final Value arg : rargs) {
                Emissions.emitArg(emit, arg, line);
            }
            return;
        }
        final List<MethodChain> chain = tokens.readChain();
        final List<Value> args = tokens.readArgs();
        if (chain.isEmpty()) {
            Emissions.openValue(emit, name, head, line);
        } else {
            Emissions.openValue(emit, null, head, line);
            emit.close();
            for (int idx = 0; idx < chain.size() - 1; idx = idx + 1) {
                final MethodChain link = chain.get(idx);
                emit.object(null, ".".concat(link.name()), line, link.dot());
                emit.method(link.fragile());
                emit.close();
            }
            final MethodChain last = chain.get(chain.size() - 1);
            emit.object(name, ".".concat(last.name()), line, last.dot());
            emit.method(last.fragile());
        }
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, line);
        }
    }

    /**
     * Open an {@code <o>} for a value as a head element. The element
     * remains open after this call so chain links or horizontal args
     * can be added inside it (or, for nested expressions, so the
     * caller can close it).
     *
     * <p>Per-kind emission:</p>
     *
     * <ul>
     *   <li>{@link Value.Kind#IDENTIFIER} — {@code <o base='<raw>'>}.</li>
     *   <li>{@link Value.Kind#INTEGER} / {@link Value.Kind#FLOAT} —
     *   {@code <o base='Φ.number'>} with a {@code <o
     *   base='Φ.bytes'>HEX&lt;/o>} child.</li>
     *   <li>{@link Value.Kind#STRING} — {@code <o base='Φ.string'>}
     *   with a {@code <o base='Φ.bytes'>HEX&lt;/o>} child carrying UTF-8
     *   bytes of the unescaped text.</li>
     *   <li>{@link Value.Kind#STAR} — {@code <o base='Φ.tuple'
     *   star=''>}.</li>
     *   <li>{@link Value.Kind#ROOT} — {@code <o base='X'>} per §9.3.</li>
     *   <li>{@link Value.Kind#GROUP} — the inner expression is parsed
     *   and emitted recursively; {@code name} attaches to its
     *   outermost {@code <o>}.</li>
     * </ul>
     *
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void openValue(
        final Emit emit, final String name, final Value value, final int line
    ) {
        if (value.kind() == Value.Kind.INTEGER || value.kind() == Value.Kind.FLOAT) {
            Emissions.number(emit, name, value, line);
        } else if (value.kind() == Value.Kind.HEX) {
            final long hex;
            try {
                hex = Long.parseLong(value.raw().substring(2), 16);
            } catch (final NumberFormatException ex) {
                final ParseError error = new ParseError(
                    line, value.pos(),
                    "hexadecimal literal is out of range"
                );
                error.initCause(ex);
                throw error;
            }
            emit.object(name, "Φ.number", line, value.pos());
            Emissions.bytesCarrier(
                emit, line, value.pos(),
                new Hex((double) hex).asString()
            );
        } else if (value.kind() == Value.Kind.BYTES) {
            emit.object(name, "Φ.bytes", line, value.pos());
            emit.object(null, null, line, value.pos());
            emit.set(value.raw());
            emit.close();
        } else if (value.kind() == Value.Kind.STRING) {
            emit.object(name, "Φ.string", line, value.pos());
            final String unescaped;
            try {
                unescaped = Emissions.unescape(value.raw());
            } catch (final NumberFormatException ex) {
                final ParseError error = new ParseError(
                    line, value.pos(), "invalid unicode or octal escape in string literal"
                );
                error.initCause(ex);
                throw error;
            }
            Emissions.bytesCarrier(
                emit, line, value.pos(),
                new Hex(unescaped).asString()
            );
        } else if (value.kind() == Value.Kind.STAR) {
            emit.object(name, "Φ.tuple", line, value.pos());
            emit.star();
        } else if (value.kind() == Value.Kind.ROOT) {
            emit.object(name, Emissions.rootBase(value.raw()), line, value.pos());
        } else if (value.kind() == Value.Kind.SELF) {
            emit.object(name, null, line, value.pos());
            emit.self();
        } else if (value.kind() == Value.Kind.TERM) {
            emit.object(name, "⊥", line, value.pos());
        } else if (value.kind() == Value.Kind.GROUP) {
            Emissions.group(emit, name, value, line);
        } else {
            emit.object(name, value.raw(), line, value.pos());
        }
    }

    /**
     * Emit a value as a self-contained argument child — opened and
     * immediately closed. If the value carries an inline binding
     * (§3.12), attaches {@code @as}.
     * @param emit Emitter
     * @param value The value
     * @param line Source line
     */
    static void emitArg(final Emit emit, final Value value, final int line) {
        final List<MethodChain> tail = value.chain();
        if (tail.isEmpty()) {
            Emissions.openValue(emit, null, value, line);
            if (value.binding() != null) {
                emit.slot(Emissions.bindingTag(value.binding()));
            }
            emit.close();
        } else {
            Emissions.openValue(emit, null, value, line);
            emit.close();
            for (int idx = 0; idx < tail.size() - 1; idx = idx + 1) {
                final MethodChain link = tail.get(idx);
                emit.object(null, ".".concat(link.name()), line, link.dot());
                emit.method(link.fragile());
                emit.close();
            }
            final MethodChain last = tail.get(tail.size() - 1);
            emit.object(null, ".".concat(last.name()), line, last.dot());
            emit.method(last.fragile());
            if (value.binding() != null) {
                emit.slot(Emissions.bindingTag(value.binding()));
            }
            emit.close();
        }
    }

    /**
     * Translate an inline-binding label to its {@code @as} value.
     * Numeric bindings become {@code αN}; identifier bindings are
     * emitted verbatim per R-9.4 inline-binding row.
     * @param raw Binding label or N
     * @return The {@code @as} attribute value
     */
    static String bindingTag(final String raw) {
        final String tag;
        if (!raw.isEmpty() && raw.chars().allMatch(c -> c >= '0' && c <= '9')) {
            tag = "α".concat(raw);
        } else {
            tag = raw;
        }
        return tag;
    }

    /**
     * Whether a head value can carry a {@code .method} chain.
     * @param head The head value
     * @return True if chain may follow
     */
    static boolean chainable(final Value head) {
        return head.kind() == Value.Kind.IDENTIFIER
            || head.kind() == Value.Kind.ROOT
            || head.kind() == Value.Kind.SELF
            || head.kind() == Value.Kind.GROUP
            || head.kind() == Value.Kind.INTEGER
            || head.kind() == Value.Kind.FLOAT
            || head.kind() == Value.Kind.STRING
            || head.kind() == Value.Kind.BYTES;
    }

    /**
     * Emit the inner {@code <o base='Φ.bytes'><o>HEX&lt;/o>&lt;/o>}
     * data carrier used by numeric, hex and string literals to hold
     * the IEEE-754/UTF-8 byte representation. The cursor is left back
     * at the parent (both nested elements are closed).
     * @param emit Emitter
     * @param line Source line
     * @param pos Source column
     * @param hex Pre-formatted hex string (BB-BB-... or empty form)
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void bytesCarrier(
        final Emit emit, final int line, final int pos, final String hex
    ) {
        emit.object(null, "Φ.bytes", line, pos);
        emit.object(null, null, line, pos);
        emit.set(hex);
        emit.close();
        emit.close();
    }

    /**
     * Decode escape sequences from a raw string body (without
     * surrounding quotes). Supports {@code \n}, {@code \t}, {@code \r},
     * {@code \b}, {@code \f}, {@code \"}, {@code \'}, {@code \\},
     * {@code \\uXXXX} unicode, and {@code \NNN} octal escapes.
     * @param inner Source body (no quotes)
     * @return Decoded text
     */
    static String unescapeBody(final String inner) {
        final StringBuilder out = new StringBuilder(inner.length());
        int idx = 0;
        while (idx < inner.length()) {
            final char glyph = inner.charAt(idx);
            if (glyph != '\\' || idx + 1 >= inner.length()) {
                out.append(glyph);
                idx = idx + 1;
                continue;
            }
            final char next = inner.charAt(idx + 1);
            if (next == 'u') {
                idx = Emissions.appendUnicode(out, inner, idx + 1);
            } else if (next >= '0' && next <= '7') {
                idx = Emissions.appendOctal(out, inner, idx + 1);
            } else {
                out.append(Emissions.singleCharEscape(glyph, next));
                idx = idx + 2;
            }
        }
        return out.toString();
    }

    /**
     * Emit an {@code INTEGER} or {@code FLOAT} as {@code Φ.number},
     * rejecting literals whose exact decimal value differs from the
     * IEEE-754 double they parse to (dead trailing digits). Alternate
     * spellings of the same value ({@code +42}, {@code 1.50}) are kept.
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value Integer or float value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static void number(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final double parsed = Double.parseDouble(value.raw());
        if (!Double.isFinite(parsed) || Emissions.overPrecise(value.raw(), parsed)) {
            final String canonical;
            if (value.kind() == Value.Kind.INTEGER) {
                canonical = Emissions.canonicalInteger(parsed);
            } else {
                canonical = Double.toString(parsed);
            }
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is over-precise, write %s instead",
                    value.raw(), canonical
                )
            );
        }
        emit.object(name, "Φ.number", line, value.pos());
        Emissions.bytesCarrier(
            emit, line, value.pos(),
            new Hex(parsed).asString()
        );
    }

    /**
     * Whether {@code raw} claims a decimal value the parsed double does
     * not hold. Accepts any spelling of the exact binary value
     * ({@code +42}, {@code 1.50}, full digit forms of exact powers of
     * two) and any spelling of {@link Double#toString(double)}'s
     * shortest form ({@code 0.1}); rejects only dead extra digits.
     * @param raw Source literal text
     * @param parsed Result of {@code Double.parseDouble(raw)}
     * @return True when the literal is over-precise
     */
    @SuppressWarnings({
        "PMD.AvoidDecimalLiteralsInBigDecimalConstructor",
        "java:S2111"
    })
    private static boolean overPrecise(final String raw, final double parsed) {
        final BigDecimal written = new BigDecimal(raw);
        return written.compareTo(new BigDecimal(parsed)) != 0
            && written.compareTo(BigDecimal.valueOf(parsed)) != 0;
    }

    /**
     * Suggested replacement spelling for a whole-valued double.
     * Mirrors the integer branch of the printer's {@code StUnhex.number},
     * except the large-magnitude arm keeps {@code Double.toString}'s
     * uppercase {@code E} so the suggestion itself re-parses as FLOAT.
     * @param num Parsed numeric value
     * @return Shortest integer-oriented spelling for {@code num}
     */
    private static String canonicalInteger(final double num) {
        final String str;
        if (Double.isFinite(num) && "-0.0".equals(Double.toString(num))) {
            str = "-0";
        } else if (Double.isFinite(num) && Math.abs(num) < 0x1p63) {
            str = Long.toString((long) num);
        } else {
            str = Double.toString(num);
        }
        return str;
    }

    /**
     * Emit a parenthesised group value, either as an inline-phi formation or
     * as a nested expression.
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The group value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static void group(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final String inner = value.raw().substring(1, value.raw().length() - 1);
        final int phi = Emissions.topLevelInlinePhi(inner);
        if (phi >= 0) {
            Emissions.inlinePhi(emit, name, inner, phi, value.pos() + 1, line);
        } else {
            final Span sub = new Span(
                " ".repeat(value.pos() + 1).concat(inner), line
            );
            Emissions.expression(emit, name, new Tokens(sub.body(), sub), line);
        }
    }

    /**
     * Map a source root character to its XMIR symbol per §9.3.
     * @param raw Source character (one of {@code Q}, {@code @},
     *  {@code ^}, {@code $})
     * @return XMIR symbol
     */
    private static String rootBase(final String raw) {
        final String mapped;
        if ("Q".equals(raw)) {
            mapped = "Φ";
        } else if ("@".equals(raw)) {
            mapped = "φ";
        } else if ("^".equals(raw)) {
            mapped = "ρ";
        } else if ("$".equals(raw)) {
            mapped = "ξ";
        } else {
            mapped = raw;
        }
        return mapped;
    }

    /**
     * Unescape a string literal's body. The {@code raw} text is the
     * source form including surrounding quotes; the returned string is
     * the decoded content.
     * @param raw Source text including the surrounding quotes
     * @return Decoded text
     */
    private static String unescape(final String raw) {
        return Emissions.unescapeBody(raw.substring(1, raw.length() - 1));
    }

    /**
     * Decode an octal escape {@code \NNN} (1 to 3 octal digits)
     * starting at the first digit position and append the resulting
     * codepoint to {@code out}.
     * @param out Output buffer
     * @param body String body
     * @param start Index of the first octal digit
     * @return Index past the consumed digits
     */
    private static int appendOctal(
        final StringBuilder out, final String body, final int start
    ) {
        int cursor = start;
        int value = 0;
        while (cursor < body.length()
            && cursor < start + 3
            && body.charAt(cursor) >= '0' && body.charAt(cursor) <= '7') {
            value = value * 8 + body.charAt(cursor) - '0';
            cursor = cursor + 1;
        }
        if (value > Emissions.MAX_OCTAL_BYTE) {
            throw new NumberFormatException(
                String.format(
                    "octal escape \\%s is out of range: value %d exceeds the 1-byte limit of 0o377 (255)",
                    body.substring(start, cursor), value
                )
            );
        }
        out.append((char) value);
        return cursor;
    }

    /**
     * Decode the body of a {@code \\uXXXX} escape (or its legacy
     * {@code \\uu...uXXXX} form per R-9.7.3) starting at the
     * {@code 'u'} position and append the resulting codepoint to
     * {@code out}.
     * @param out Output buffer
     * @param body String body
     * @param start Index of the first {@code 'u'}
     * @return Index past the consumed escape
     */
    private static int appendUnicode(
        final StringBuilder out, final String body, final int start
    ) {
        int cursor = start;
        while (cursor < body.length() && body.charAt(cursor) == 'u') {
            cursor = cursor + 1;
        }
        if (cursor + 4 > body.length()) {
            out.append('\\').append(body, start, body.length());
        } else {
            out.append(
                (char) Integer.parseInt(body.substring(cursor, cursor + 4), 16)
            );
        }
        return cursor + 4;
    }

    /**
     * Resolve a single-character escape sequence (e.g. {@code \n},
     * {@code \t}). Unknown sequences are passed through verbatim.
     * @param head Backslash character (always {@code '\\'})
     * @param next The character after the backslash
     * @return The decoded character(s)
     */
    private static String singleCharEscape(final char head, final char next) {
        final String decoded;
        if (next == 'n') {
            decoded = String.valueOf((char) 10);
        } else if (next == 't') {
            decoded = String.valueOf((char) 9);
        } else if (next == 'r') {
            decoded = String.valueOf((char) 13);
        } else if (next == 'b') {
            decoded = String.valueOf((char) 8);
        } else if (next == 'f') {
            decoded = String.valueOf((char) 12);
        } else if (next == '"' || next == '\'' || next == '\\') {
            decoded = String.valueOf(next);
        } else {
            decoded = new String(new char[]{head, next});
        }
        return decoded;
    }

    /**
     * Whether the cursor is at a reversed-dispatch separator — a
     * {@code .} immediately followed by a space or end-of-body. Used
     * to recognise inner reversed-dispatch expressions inside paren
     * groups (e.g. {@code (mod. y 4)}), where the {@code name.} form
     * opens a new dispatch chain.
     * @param tokens Token reader (positioned after the head)
     * @param head Just-read head value
     * @return True when the cursor is at a reversed-dispatch dot
     */
    private static boolean reversedDispatch(final Tokens tokens, final Value head) {
        final boolean reversed;
        if (head.kind() == Value.Kind.IDENTIFIER
            && !tokens.atEnd() && tokens.current() == '.') {
            final int probe = tokens.cursor() + 1;
            reversed = probe >= tokens.body().length()
                || tokens.body().charAt(probe) == ' ';
        } else {
            reversed = false;
        }
        return reversed;
    }

    /**
     * Find the position of a top-level {@code > [} inline-phi marker
     * in {@code body} (depth-zero, not inside strings). Returns the
     * index of the {@code >} char, or {@code -1} if none.
     * @param body The inner body of a paren group
     * @return Index of {@code >} char, or {@code -1}
     */
    private static int topLevelInlinePhi(final String body) {
        int depth = 0;
        boolean instr = false;
        int found = -1;
        int idx = 0;
        while (idx < body.length() - 2 && found < 0) {
            final char glyph = body.charAt(idx);
            if (instr) {
                if (glyph == '\\' && idx + 1 < body.length()) {
                    idx = idx + 1;
                } else if (glyph == '"') {
                    instr = false;
                }
            } else if (glyph == '"') {
                instr = true;
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            } else if (depth == 0 && glyph == '>'
                && body.charAt(idx + 1) == ' ' && body.charAt(idx + 2) == '[') {
                found = idx;
            }
            idx = idx + 1;
        }
        return found;
    }

    /**
     * Emit an inline-phi formation (§3.10) detected inside a paren
     * group. The formation is anonymous (no {@code > name} suffix);
     * its {@code φ} slot holds the body expression.
     * @param emit Emitter
     * @param name Name to attach to the formation's {@code <o>}
     * @param inner The full inner body of the paren group
     * @param phi Index of the {@code >} that begins {@code > [}
     * @param column Absolute source column of the first body char
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static void inlinePhi(
        final Emit emit, final String name, final String inner,
        final int phi, final int column, final int line
    ) {
        final int bracket = phi + 2;
        final int close = inner.indexOf(']', bracket);
        if (close < 0) {
            throw new ParseError(
                line, column + bracket,
                "only-phi parameter list missing closing `]`"
            );
        }
        final String lhs = inner.substring(0, phi).stripTrailing();
        final String params = inner.substring(bracket + 1, close);
        emit.object(name, null, line, column);
        int pcol = column + bracket + 1;
        for (final String param : Emissions.splitParams(params)) {
            final String mapped;
            if (param.equals("@")) {
                mapped = "φ";
            } else {
                mapped = param;
            }
            emit.voidParam(mapped, line, pcol);
            pcol = pcol + param.length() + 1;
        }
        final Span sub = new Span(" ".repeat(column).concat(lhs), line);
        Emissions.expression(emit, "φ", new Tokens(sub.body(), sub), line);
        emit.close();
    }

    /**
     * Split a parameter-list body into individual names by single
     * spaces.
     * @param text Param text (without surrounding brackets)
     * @return Names in source order
     */
    private static List<String> splitParams(final String text) {
        final List<String> out = new java.util.ArrayList<>(0);
        int idx = 0;
        while (idx < text.length()) {
            int end = idx;
            while (end < text.length() && text.charAt(end) != ' ') {
                end = end + 1;
            }
            out.add(text.substring(idx, end));
            if (end < text.length()) {
                idx = end + 1;
            } else {
                idx = end;
            }
        }
        return out;
    }
}
