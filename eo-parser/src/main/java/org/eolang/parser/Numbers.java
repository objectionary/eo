/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Numeric literal emission for {@link Emissions}.
 *
 * <p>Integer, float and hexadecimal literals all render to the same
 * {@code <o base='Φ.number'>} shape with a {@code Φ.bytes} carrier, so
 * the over-precision checks and the canonicalisation helpers live here
 * rather than bloating the {@link Emissions} facade (which trips
 * qulice's God Class rule once too many recipes pile up).</p>
 *
 * @since 0.1
 */
final class Numbers {

    /**
     * Bits an IEEE-754 double keeps below the leading one of its
     * significand.
     */
    private static final int SIGNIFICAND_BITS = 52;

    /**
     * No instances.
     */
    private Numbers() {
    }

    /**
     * Emit an integer or float literal as {@code <o base='Φ.number'>}
     * with a {@code Φ.bytes} carrier (§9.4.2).
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
        if (Numbers.overPrecise(value.raw(), parsed)) {
            final String canonical;
            if (value.kind() == Value.Kind.INTEGER) {
                canonical = Numbers.canonicalInteger(parsed);
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
        Numbers.emitNumber(emit, name, parsed, line, value.pos());
    }

    /**
     * Emit a hexadecimal literal as {@code <o base='Φ.number'>} with a
     * {@code Φ.bytes} carrier (§9.4.2).
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
            throw Emissions.numberFormat(
                ex, line, value.pos(), "hexadecimal literal is out of range"
            );
        }
        final double parsed = raw;
        if (!Numbers.exact(new BigDecimal(raw), parsed)) {
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is over-precise, write %s instead",
                    value.raw(), Numbers.canonicalInteger(parsed)
                )
            );
        }
        Numbers.emitNumber(emit, name, parsed, line, value.pos());
    }

    private static void emitNumber(
        final Emit emit, final String name, final double parsed,
        final int line, final int pos
    ) {
        emit.object(name, "Φ.number", line, pos);
        Emissions.bytesCarrier(
            emit, line, pos, new Hex(parsed).asString()
        );
    }

    private static boolean overPrecise(final String raw, final double parsed) {
        final BigDecimal written = new BigDecimal(raw);
        return !Numbers.exact(written, parsed)
            && written.compareTo(BigDecimal.valueOf(parsed)) != 0;
    }

    private static boolean exact(final BigDecimal decimal, final double value) {
        return decimal.compareTo(Numbers.exactly(value)) == 0;
    }

    private static BigDecimal exactly(final double value) {
        final int exponent = Math.max(
            Math.getExponent(value), Double.MIN_EXPONENT
        ) - Numbers.SIGNIFICAND_BITS;
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
