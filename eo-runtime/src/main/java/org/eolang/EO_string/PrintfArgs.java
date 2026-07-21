/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Phi;

/**
 * Printf arguments.
 * @since 0.57.4
 */
final class PrintfArgs {

    /**
     * Character conversion.
     */
    private static final Map<Character, Function<Dataized, Object>> CONVERSION = new HashMap<>();

    /**
     * Percent sign.
     */
    private static final char PERCENT = '%';

    /**
     * A single {@code printf} specifier. Group 1 is the optional {@code N$}
     * positional index; an optional run of flags, an optional width and an
     * optional {@code .precision} are then skipped; group 2 is the conversion
     * character. Skipping the flags, width and precision keeps exactly one
     * argument counted per specifier such as {@code %5d}, {@code %.2f} or
     * {@code %-10s}, matching what {@code String.format} consumes on the
     * formatting side. Possessive quantifiers keep the scan linear in the
     * length of the format string, with no backtracking.
     */
    private static final Pattern SPECIFIER = Pattern.compile(
        "%(\\d++\\$)?+[-#+ 0,(]*+\\d*+(?:\\.\\d++)?+([a-zA-Z%])"
    );

    /**
     * One past the largest {@code double} that still narrows to a valid
     * {@code long} without saturating: {@code 2^63}, exactly one ulp above
     * {@link Long#MAX_VALUE}. {@code (double) Long.MAX_VALUE} itself rounds
     * up to this same value, so comparing against it directly (rather than
     * against the rounded {@code Long.MAX_VALUE}) is what makes the {@code
     * >=} guard in {@link #toLong(double)} exact.
     */
    private static final double LONG_UPPER_LIMIT = 0x1.0p63;

    static {
        PrintfArgs.CONVERSION.put('s', Dataized::asString);
        PrintfArgs.CONVERSION.put('d', element -> PrintfArgs.toLong(element.asNumber()));
        PrintfArgs.CONVERSION.put('f', Dataized::asNumber);
        PrintfArgs.CONVERSION.put('x', element -> PrintfArgs.bytesToHex(element.take()));
        PrintfArgs.CONVERSION.put('b', Dataized::asBool);
    }

    /**
     * The format.
     */
    private final String format;

    /**
     * The length.
     */
    private final long length;

    /**
     * Phi attribute.
     */
    private final Phi retriever;

    /**
     * Ctor.
     * @param fmt The format
     * @param len The length
     * @param phi Phi attribute
     */
    PrintfArgs(final String fmt, final long len, final Phi phi) {
        this.format = fmt;
        this.length = len;
        this.retriever = phi;
    }

    List<Object> formatted() {
        final List<Object> arguments = new ArrayList<>(0);
        final Matcher matcher = PrintfArgs.SPECIFIER.matcher(this.format);
        long auto = 0L;
        while (matcher.find()) {
            final String positional = matcher.group(1);
            final char symbol = matcher.group(2).charAt(0);
            if (symbol == PrintfArgs.PERCENT) {
                continue;
            }
            final long arg;
            if (positional != null) {
                final String digits = positional.substring(0, positional.length() - 1);
                try {
                    arg = Long.parseLong(digits) - 1L;
                } catch (final NumberFormatException ex) {
                    throw new ExFailure(
                        String.format(
                            "The argument index %s is out of bounds (total arguments: %d)",
                            digits, this.length
                        ),
                        ex
                    );
                }
                if (arg < 0L) {
                    throw new ExFailure(
                        "The argument index %s must be a positive number (1-based) for the '%%N$' conversion",
                        digits
                    );
                }
            } else {
                arg = auto;
                auto += 1L;
            }
            if (arg >= this.length) {
                throw new ExFailure(
                    "The argument index %d is out of bounds (total arguments: %d)",
                    arg, this.length
                );
            }
            final Phi taken = this.retriever.copy();
            taken.put(0, new Data.ToPhi(arg));
            arguments.add(PrintfArgs.fmt(symbol, new Dataized(taken)));
        }
        return arguments;
    }

    /**
     * Format given {@code element} depending on format char.
     * @param symbol Format char
     * @param element Element ready for formatting
     * @return Formatted object
     */
    private static Object fmt(final char symbol, final Dataized element) {
        if (!PrintfArgs.CONVERSION.containsKey(symbol)) {
            throw new ExFailure(
                "The format %c is unsupported, only %s formats can be used",
                symbol, "%s, %d, %f, %x, %b"
            );
        }
        return PrintfArgs.CONVERSION.get(symbol).apply(element);
    }

    /**
     * Convert a number to {@code long} for the {@code %d} conversion, rejecting
     * a value outside {@code long} range instead of silently saturating to
     * {@link Long#MAX_VALUE}/{@link Long#MIN_VALUE} as {@link Double#longValue()} does.
     * @param number Number to convert
     * @return The number as {@code long}
     */
    private static long toLong(final double number) {
        if (number < Long.MIN_VALUE || number >= PrintfArgs.LONG_UPPER_LIMIT) {
            throw new ExFailure(
                "The number %s doesn't fit into long range for the '%%d' conversion",
                number
            );
        }
        return (long) number;
    }

    /**
     * Convert byte array to hex string.
     * @param bytes Byte array
     * @return Bytes as hex string
     */
    private static String bytesToHex(final byte[] bytes) {
        final StringJoiner out = new StringJoiner("-");
        for (final byte bty : bytes) {
            out.add(String.format("%02X", bty));
        }
        return out.toString();
    }
}
