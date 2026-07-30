/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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
     * positional index; group 2 is an optional run of flags, an optional width
     * and an optional {@code .precision}; group 3 is the conversion character.
     * Skipping the flags, width and precision keeps exactly one
     * argument counted per specifier such as {@code %5d}, {@code %.2f} or
     * {@code %-10s}, matching what {@code String.format} consumes on the
     * formatting side. Possessive quantifiers keep the scan linear in the
     * length of the format string, with no backtracking.
     */
    private static final Pattern SPECIFIER = Pattern.compile(
        "%(\\d++\\$)?+([-#+ 0,(]*+\\d*+(?:\\.\\d++)?+)([a-zA-Z%])"
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

    /**
     * Formas whose bytes are a value, never text, and so can never be the
     * argument of a {@code %s} conversion.
     *
     * <p>Naming what text is <em>not</em>, rather than what it is, is
     * deliberate. Only a literal has the forma {@code Φ.string}: every
     * operation that builds a string returns something else
     * ({@code "a".concat "b"} normalizes to {@code Φ.bytes}, {@code slice}
     * to {@code Φ.string.slice}), and so does any user object that
     * decorates a string. Demanding {@code Φ.string} would refuse all of
     * those, which is a far bigger problem than the one this guard is
     * here for.</p>
     */
    private static final Set<String> NOT_TEXT = new HashSet<>(
        Arrays.asList("Φ.number", "Φ.bool")
    );

    static {
        PrintfArgs.CONVERSION.put('s', PrintfArgs::validString);
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
     * The tuple of arguments.
     */
    private final Phi args;

    /**
     * Ctor.
     * @param fmt The format
     * @param len The length
     * @param tuple The tuple of arguments
     */
    PrintfArgs(final String fmt, final long len, final Phi tuple) {
        this.format = fmt;
        this.length = len;
        this.args = tuple;
    }

    /**
     * Adapt an EO printf format for {@link String#format(String, Object...)}.
     * @param format Format
     * @return Java format
     */
    static String javaFormat(final String format) {
        final Matcher matcher = PrintfArgs.SPECIFIER.matcher(format);
        final StringBuilder converted = new StringBuilder(format.length());
        while (matcher.find()) {
            final char symbol = matcher.group(3).charAt(0);
            final char conversion;
            if (symbol == 'x') {
                conversion = 's';
            } else {
                conversion = symbol;
            }
            final String replacement = new StringBuilder(2 + matcher.group(2).length())
                .append(PrintfArgs.PERCENT)
                .append(matcher.group(2))
                .append(conversion)
                .toString();
            matcher.appendReplacement(
                converted,
                Matcher.quoteReplacement(replacement)
            );
        }
        matcher.appendTail(converted);
        return converted.toString();
    }

    List<Object> formatted() {
        final List<Object> arguments = new ArrayList<>(0);
        final Matcher matcher = PrintfArgs.SPECIFIER.matcher(this.format);
        long auto = 0L;
        while (matcher.find()) {
            final String positional = matcher.group(1);
            final char symbol = matcher.group(3).charAt(0);
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
            arguments.add(PrintfArgs.fmt(symbol, this.element(arg)));
        }
        return arguments;
    }

    /**
     * The argument at the given index, as the object the caller passed in
     * rather than the one {@code tuple.at} hands back.
     *
     * <p>The difference is the whole point: {@code at} decorates what it
     * returns, so the result reports {@code Φ.tuple.at} as its forma and
     * the argument's own type is no longer visible. Walking the cons list
     * reaches the object itself, forma intact. The list is built with the
     * last argument at the head (see {@code tuple.eo}), so the walk takes
     * as many tails as there are arguments after this one, counted off the
     * tuple's own length so that a wrong count can only pick the wrong
     * argument, never walk off the end into a terminated computation.</p>
     *
     * @param index Zero-based index of the argument
     * @return The argument
     */
    private Phi element(final long index) {
        Phi current = this.args;
        final long size = new Dataized(this.args.take("length")).asNumber().longValue();
        for (long step = size - 1L - index; step > 0L; --step) {
            current = current.take("tail");
        }
        return current.take("head");
    }

    /**
     * Format given {@code element} depending on format char.
     * @param symbol Format char
     * @param element Element ready for formatting
     * @return Formatted object
     */
    private static Object fmt(final char symbol, final Phi element) {
        if (!PrintfArgs.CONVERSION.containsKey(symbol)) {
            throw new ExFailure(
                "The format %c is unsupported, only %s formats can be used",
                symbol, "%s, %d, %f, %x, %b"
            );
        }
        if (symbol == 's') {
            PrintfArgs.textual(element);
        }
        return PrintfArgs.CONVERSION.get(symbol).apply(new Dataized(element));
    }

    /**
     * Refuse an argument whose type says its bytes are a value and not text.
     *
     * <p>Without this, a {@code number} handed to {@code %s} is decoded as
     * if its eight raw IEEE-754 bytes were UTF-8, and {@code true} prints
     * as a control character, both with no complaint. Only bytes that are
     * not valid UTF-8 at all were caught before, which is a matter of luck
     * rather than of type.</p>
     *
     * @param element The argument
     */
    private static void textual(final Phi element) {
        final String forma = element.normalized().forma();
        if (PrintfArgs.NOT_TEXT.contains(forma)) {
            throw new ExFailure(
                "The argument of the '%%s' conversion is %s, whose bytes are a value and not text; use %s instead",
                forma, "'%d', '%f', '%b' or '%x'"
            );
        }
    }

    /**
     * Convert the {@code element} to a string for the {@code %s} conversion,
     * rejecting bytes that are not valid UTF-8 instead of silently decoding
     * an arbitrary byte sequence (e.g. the raw IEEE-754 bytes of a {@code
     * number} argument) into mojibake with no error, as {@link
     * Dataized#asString()} does.
     * @param element Element ready for formatting
     * @return The element as a string
     */
    private static String validString(final Dataized element) {
        final byte[] bytes = element.take();
        try {
            return StandardCharsets.UTF_8.newDecoder()
                .onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT)
                .decode(ByteBuffer.wrap(bytes))
                .toString();
        } catch (final CharacterCodingException ex) {
            throw new ExFailure(
                String.format(
                    "Can't convert %d bytes to a string for the '%%s' conversion, not valid UTF-8",
                    bytes.length
                ),
                ex
            );
        }
    }

    /**
     * Convert a number to {@code long} for the {@code %d} conversion, rejecting
     * a value outside {@code long} range instead of silently saturating to
     * {@link Long#MAX_VALUE}/{@link Long#MIN_VALUE} as {@link Double#longValue()} does,
     * and rejecting {@link Double#NaN} instead of letting it slip past both bounds
     * (every relational comparison against {@code NaN} is {@code false}) and narrow
     * to {@code 0} per JLS 5.1.3.
     * @param number Number to convert
     * @return The number as {@code long}
     */
    private static long toLong(final double number) {
        if (
            Double.isNaN(number)
                || number < Long.MIN_VALUE
                || number >= PrintfArgs.LONG_UPPER_LIMIT
        ) {
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
