/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Set;

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
 */
final class Emissions {

    /**
     * Maximum value of a {@code \NNN} octal byte escape (0o377, one byte).
     */
    private static final int MAX_OCTAL_BYTE = 0xFF;

    /**
     * Bits an IEEE-754 double keeps below the leading one of its
     * significand.
     */
    private static final int SIGNIFICAND_BITS = 52;

    /**
     * The void the identity object {@code I} binds and decorates.
     */
    private static final String IDENTITY = "x";

    /**
     * Kinds of head value that a {@code .method} chain may follow.
     */
    private static final Set<Value.Kind> CHAINABLE = Set.of(
        Value.Kind.IDENTIFIER,
        Value.Kind.ROOT,
        Value.Kind.SELF,
        Value.Kind.GROUP,
        Value.Kind.INTEGER,
        Value.Kind.FLOAT,
        Value.Kind.STRING,
        Value.Kind.BYTES
    );

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
        ChainEmission.link(emit, line, head, chain, name);
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
     * <li>{@link Value.Kind#IDENTIFIER} — {@code <o base='<raw>'>}.</li>
     * <li>{@link Value.Kind#INTEGER} / {@link Value.Kind#FLOAT} —
     * {@code <o base='Φ.number'>} with a {@code <o
     * base='Φ.bytes'>HEX</o>} child.</li>
     * <li>{@link Value.Kind#STRING} — {@code <o base='Φ.string'>}
     * with a {@code <o base='Φ.bytes'>HEX</o>} child carrying UTF-8
     * bytes of the unescaped text.</li>
     * <li>{@link Value.Kind#STAR} — {@code <o base='Φ.tuple'
     * star=''>}.</li>
     * <li>{@link Value.Kind#ROOT} — {@code <o base='X'>} per §9.3.</li>
     * <li>{@link Value.Kind#GROUP} — the inner expression is parsed
     * and emitted recursively; {@code name} attaches to its
     * outermost {@code <o>}.</li>
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
            Emissions.hex(emit, name, value, line);
        } else if (value.kind() == Value.Kind.BYTES) {
            emit.object(name, "Φ.bytes", line, value.pos());
            emit.object(null, null, line, value.pos());
            emit.set(value.raw());
            emit.close();
        } else if (value.kind() == Value.Kind.STRING) {
            Emissions.string(emit, name, value, line);
        } else {
            Emissions.openBase(emit, name, value, line);
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
            if (value.bound()) {
                emit.slot(Emissions.bindingTag(value.binding()));
            }
            if (value.constant()) {
                emit.constant();
            }
            emit.close();
        } else {
            ChainEmission.link(emit, line, value, tail, null);
            if (value.bound()) {
                emit.slot(Emissions.bindingTag(value.binding()));
            }
            if (value.constant()) {
                emit.constant();
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
        } else if ("^".equals(raw)) {
            tag = "ρ";
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
        return Emissions.CHAINABLE.contains(head.kind());
    }

    /**
     * Emit the inner {@code <o base='Φ.bytes'><o>HEX</o></o>}
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
     * Decode a string body to its raw byte representation.
     * @param inner Source body without surrounding quotes
     * @return Decoded bytes
     */
    static byte[] unescapeBytes(final String inner) {
        return Emissions.unescapeRawBytes(inner);
    }

    private static void openBase(
        final Emit emit, final String name, final Value value, final int line
    ) {
        if (value.kind() == Value.Kind.STAR) {
            emit.object(name, "Φ.tuple", line, value.pos());
            emit.star();
        } else if (value.kind() == Value.Kind.ROOT) {
            emit.object(name, Emissions.rootBase(value.raw()), line, value.pos());
        } else if (value.kind() == Value.Kind.SELF) {
            emit.object(name, null, line, value.pos());
            emit.self();
        } else if (value.kind() == Value.Kind.TERM) {
            emit.object(name, "⊥", line, value.pos());
        } else if (value.kind() == Value.Kind.IDENTITY) {
            Emissions.identity(emit, name, value, line);
        } else if (value.kind() == Value.Kind.GROUP) {
            Emissions.group(emit, name, value, line);
        } else {
            emit.object(name, value.raw(), line, value.pos());
        }
    }

    private static void identity(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.object(name, null, line, value.pos());
        emit.voidParam(Emissions.IDENTITY, line, value.pos());
        emit.object("φ", Emissions.IDENTITY, line, value.pos());
        emit.close();
    }

    private static void hex(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final long raw;
        try {
            raw = Long.parseLong(value.raw().substring(2), 16);
        } catch (final NumberFormatException ex) {
            final ParseError error = new ParseError(
                line, value.pos(),
                "hexadecimal literal is out of range"
            );
            error.initCause(ex);
            throw error;
        }
        final double parsed = raw;
        if (!Emissions.exact(new BigDecimal(raw), parsed)) {
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is over-precise, write %s instead",
                    value.raw(), Emissions.canonicalInteger(parsed)
                )
            );
        }
        emit.object(name, "Φ.number", line, value.pos());
        Emissions.bytesCarrier(
            emit, line, value.pos(),
            new Hex(parsed).asString()
        );
    }

    private static void string(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.object(name, "Φ.string", line, value.pos());
        final byte[] unescaped;
        try {
            unescaped = Emissions.unescapeBytes(
                value.raw().substring(1, value.raw().length() - 1)
            );
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
    }

    private static void rejectLoneSurrogates(final CharSequence text) {
        int cursor = 0;
        while (cursor < text.length()) {
            final char glyph = text.charAt(cursor);
            if (Character.isHighSurrogate(glyph)
                && cursor + 1 < text.length()
                && Character.isLowSurrogate(text.charAt(cursor + 1))) {
                cursor = cursor + 2;
                continue;
            }
            if (Character.isSurrogate(glyph)) {
                throw new NumberFormatException(
                    String.format(
                        "unicode escape \\u%04X is a lone surrogate, not a valid standalone codepoint",
                        (int) glyph
                    )
                );
            }
            cursor = cursor + 1;
        }
    }

    private static void number(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final double parsed = Double.parseDouble(value.raw());
        if (!Double.isFinite(parsed)) {
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is out of the finite range of a double", value.raw()
                )
            );
        }
        if (Emissions.overPrecise(value.raw(), parsed)) {
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

    private static boolean overPrecise(final String raw, final double parsed) {
        final BigDecimal written = new BigDecimal(raw);
        return !Emissions.exact(written, parsed)
            && written.compareTo(BigDecimal.valueOf(parsed)) != 0;
    }

    private static boolean exact(final BigDecimal decimal, final double value) {
        return decimal.compareTo(Emissions.exactly(value)) == 0;
    }

    private static BigDecimal exactly(final double value) {
        final int exponent = Math.max(
            Math.getExponent(value), Double.MIN_EXPONENT
        ) - Emissions.SIGNIFICAND_BITS;
        final BigInteger mantissa = BigInteger.valueOf(
            (long) Math.scalb(value, -exponent)
        );
        final BigDecimal exact;
        if (exponent < 0) {
            exact = new BigDecimal(
                mantissa.multiply(BigInteger.valueOf(5L).pow(-exponent)),
                -exponent
            );
        } else {
            exact = new BigDecimal(mantissa.shiftLeft(exponent));
        }
        return exact;
    }

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

    private static byte[] unescapeRawBytes(final String inner) {
        final ByteArrayOutputStream out = new ByteArrayOutputStream(inner.length());
        final StringBuilder text = new StringBuilder(inner.length());
        int idx = 0;
        while (idx < inner.length()) {
            final char glyph = inner.charAt(idx);
            if (glyph != '\\' || idx + 1 >= inner.length()) {
                text.append(glyph);
                idx = idx + 1;
                continue;
            }
            final char next = inner.charAt(idx + 1);
            if (next == 'u') {
                idx = Emissions.appendUnicode(text, inner, idx + 1);
            } else if (next >= '0' && next <= '7') {
                idx = Emissions.rawOctal(out, text, inner, idx + 1);
            } else {
                text.append(Emissions.singleCharEscape(glyph, next));
                idx = idx + 2;
            }
        }
        Emissions.appendText(out, text);
        return out.toByteArray();
    }

    private static int rawOctal(
        final ByteArrayOutputStream out, final StringBuilder text,
        final String body, final int start
    ) {
        Emissions.appendText(out, text);
        int cursor = start;
        int value = 0;
        while (cursor < body.length() && cursor < start + 3
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
        out.write(value);
        return cursor;
    }

    private static void appendText(
        final ByteArrayOutputStream out, final StringBuilder text
    ) {
        Emissions.rejectLoneSurrogates(text);
        final byte[] bytes = text.toString().getBytes(StandardCharsets.UTF_8);
        out.write(bytes, 0, bytes.length);
        text.setLength(0);
    }

    private static int appendUnicode(
        final StringBuilder out, final String body, final int start
    ) {
        int cursor = start;
        while (cursor < body.length() && body.charAt(cursor) == 'u') {
            cursor = cursor + 1;
        }
        boolean valid = cursor + 4 <= body.length();
        for (int idx = cursor; valid && idx < cursor + 4; idx = idx + 1) {
            valid = Character.digit(body.charAt(idx), 16) >= 0;
        }
        if (!valid) {
            throw new NumberFormatException(
                String.format(
                    "unicode escape \\%s is not exactly four hexadecimal digits",
                    body.substring(start, Math.min(body.length(), cursor + 4))
                )
            );
        }
        out.append(
            (char) Integer.parseInt(body.substring(cursor, cursor + 4), 16)
        );
        return cursor + 4;
    }

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
            throw new NumberFormatException(
                String.format("unrecognised escape sequence '%c%c'", head, next)
            );
        }
        return decoded;
    }

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

    private static int topLevelInlinePhi(final String body) {
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
            } else if (depth == 0 && glyph == '>'
                && body.charAt(idx + 1) == ' ' && body.charAt(idx + 2) == '[') {
                found = idx;
            }
            idx = idx + 1;
        }
        return found;
    }

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
            if ("@".equals(param)) {
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
