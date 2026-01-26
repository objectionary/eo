/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOtt; // NOPMD

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
 * Sprintf arguments.
 * @since 0.57.4
 */
final class SprintfArgs {
    /**
     * Character conversion.
     */
    private static final Map<Character, Function<Dataized, Object>> CONVERSION = new HashMap<>();

    /**
     * Percent sign.
     */
    private static final char PERCENT = '%';

    static {
        SprintfArgs.CONVERSION.put('s', Dataized::asString);
        SprintfArgs.CONVERSION.put('d', element -> element.asNumber().longValue());
        SprintfArgs.CONVERSION.put('f', Dataized::asNumber);
        SprintfArgs.CONVERSION.put('x', element -> SprintfArgs.bytesToHex(element.take()));
        SprintfArgs.CONVERSION.put('b', Dataized::asBool);
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
    SprintfArgs(final String fmt, final long len, final Phi phi) {
        this.format = fmt;
        this.length = len;
        this.retriever = phi;
    }

    public List<Object> formatted() {
        final List<Object> arguments = new ArrayList<>(0);
        final Matcher matcher = Pattern.compile("%(\\d+\\$)?([a-zA-Z%])").matcher(this.format);
        long auto = 0L;
        while (matcher.find()) {
            final String positional = matcher.group(1);
            final char symbol = matcher.group(2).charAt(0);
            if (symbol == SprintfArgs.PERCENT) {
                continue;
            }
            final long arg;
            if (positional != null) {
                arg = Long.parseLong(positional.substring(0, positional.length() - 1)) - 1L;
            } else {
                arg = auto;
                auto += 1L;
            }
            if (arg >= this.length) {
                throw new ExFailure(
                    String.format(
                        "The argument index %d is out of bounds (total arguments: %d)",
                        arg, this.length
                    )
                );
            }
            final Phi taken = this.retriever.copy();
            taken.put(0, new Data.ToPhi(arg));
            arguments.add(SprintfArgs.fmt(symbol, new Dataized(taken)));
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
        if (!SprintfArgs.CONVERSION.containsKey(symbol)) {
            throw new ExFailure(
                String.format(
                    "The format %c is unsupported, only %s formats can be used",
                    symbol, "%s, %d, %f, %x, %b"
                )
            );
        }
        return SprintfArgs.CONVERSION.get(symbol).apply(element);
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
