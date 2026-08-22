/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Rendering recipes for literal values (numbers, hex, strings, groups,
 * roots, identity and tuple heads) into XMIR.
 *
 * <p>Pulled out of {@link Emissions} so that file stays a thin facade
 * over the literal-rendering recipes. Each method emits its own
 * {@code <o>} (and, for numeric/string literals, the nested
 * {@code Φ.bytes} carrier) per §9.4 / §9.4.2.</p>
 *
 * @since 0.1
 */
final class Literals {

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
     * No instances.
     */
    private Literals() {
    }

    /**
     * Emit an integer or float literal as {@code <o base='Φ.number'>}
     * with a {@code Φ.bytes} child holding the IEEE-754 bytes.
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void number(
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
        if (Literals.overPrecise(value.raw(), parsed)) {
            final String canonical;
            if (value.kind() == Value.Kind.INTEGER) {
                canonical = Literals.canonicalInteger(parsed);
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
     * Emit a {@code 0x…} hex literal as a number with its byte carrier.
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void hex(
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
        if (!Literals.exact(new BigDecimal(raw), parsed)) {
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is over-precise, write %s instead",
                    value.raw(), Literals.canonicalInteger(parsed)
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
     * Emit a string literal as {@code <o base='Φ.string'>} with a
     * {@code Φ.bytes} child holding the UTF-8 bytes of the unescaped text.
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void string(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.object(name, "Φ.string", line, value.pos());
        final byte[] unescaped;
        try {
            unescaped = ByteEscapes.unescapeRawBytes(
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

    /**
     * Emit the base {@code <o>} for a non-numeric, non-string value
     * (identifier, star, root, term, identity, group).
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void openBase(
        final Emit emit, final String name, final Value value, final int line
    ) {
        if (value.kind() == Value.Kind.STAR) {
            emit.object(name, "Φ.tuple", line, value.pos());
            emit.star();
        } else if (value.kind() == Value.Kind.ROOT) {
            emit.object(name, Literals.rootBase(value.raw()), line, value.pos());
        } else if (value.kind() == Value.Kind.TERM) {
            emit.object(name, "⊥", line, value.pos());
        } else if (value.kind() == Value.Kind.IDENTITY) {
            Literals.identity(emit, name, value, line);
        } else if (value.kind() == Value.Kind.GROUP) {
            Literals.group(emit, name, value, line);
        } else {
            emit.object(name, value.raw(), line, value.pos());
        }
    }

    private static void identity(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.object(name, null, line, value.pos());
        emit.voidParam(Literals.IDENTITY, line, value.pos());
        emit.object("φ", Literals.IDENTITY, line, value.pos());
        emit.close();
    }

    private static void group(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final String inner = value.raw().substring(1, value.raw().length() - 1);
        final int phi = InlinePhi.topLevelInlinePhi(inner);
        if (phi >= 0) {
            InlinePhi.inlinePhi(emit, name, inner, phi, value.pos() + 1, line);
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

    private static boolean overPrecise(final String raw, final double parsed) {
        final BigDecimal written = new BigDecimal(raw);
        return !Literals.exact(written, parsed)
            && written.compareTo(BigDecimal.valueOf(parsed)) != 0;
    }

    private static boolean exact(final BigDecimal decimal, final double value) {
        return decimal.compareTo(Literals.exactly(value)) == 0;
    }

    private static BigDecimal exactly(final double value) {
        final int exponent = Math.max(
            Math.getExponent(value), Double.MIN_EXPONENT
        ) - Literals.SIGNIFICAND_BITS;
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
}
