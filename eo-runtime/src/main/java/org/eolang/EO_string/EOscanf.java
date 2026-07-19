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
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Attrs;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Scanf.
 * @since 0.39
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "scanf")
public final class EOscanf extends PhDefault implements Atom {

    /**
     * Character conversion.
     */
    private static final Map<Character, Function<String, Phi>> CONVERSION = new HashMap<>();

    static {
        EOscanf.CONVERSION.put('s', ToPhi::new);
        EOscanf.CONVERSION.put('d', str -> new Data.ToPhi(EOscanf.parsed(str)));
        EOscanf.CONVERSION.put('f', str -> new Data.ToPhi(Double.parseDouble(str)));
    }

    /**
     * Percent sign.
     */
    private static final char PERCENT = '%';

    /**
     * Ctor.
     * @checkstyle CyclomaticComplexityCheck (75 lines)
     * @checkstyle NestedIfDepthCheck (75 lines)
     */
    public EOscanf() {
        super(new Attrs(
            new Attr("format", new AtVoid("format")),
            new Attr("read", new AtVoid("read"))
        ));
    }

    @Override
    @SuppressWarnings("PMD.CognitiveComplexity")
    public Phi lambda() {
        final String format = new Dataized(this.take("format")).asString();
        final StringBuilder regex = new StringBuilder(64);
        boolean literal = false;
        for (int idx = 0; idx < format.length(); ++idx) {
            final char sym = format.charAt(idx);
            if (sym == EOscanf.PERCENT) {
                if (literal) {
                    regex.append(EOscanf.PERCENT);
                    literal = false;
                } else {
                    literal = true;
                }
            } else {
                if (literal) {
                    switch (sym) {
                        case 'd':
                            regex.append("([+-]?\\d+)");
                            break;
                        case 'f':
                            regex.append("([+-]?\\d+(?:\\.\\d+)?)");
                            break;
                        case 's':
                            regex.append("(\\S+)");
                            break;
                        default:
                            throw new ExFailure("Unsupported format specifier: '%%%c'", sym);
                    }
                    literal = false;
                } else {
                    regex.append(Pattern.quote(String.valueOf(sym)));
                }
            }
        }
        if (literal) {
            throw new ExFailure(
                "The format string '%s' ends with a dangling '%%' and no specifier after it",
                format
            );
        }
        final Matcher matcher = Pattern.compile(regex.toString()).matcher(
            new Dataized(this.take("read")).asString()
        );
        String frmt = format;
        final List<Phi> output = new ArrayList<>(0);
        if (matcher.find()) {
            int index = 1;
            while (true) {
                final int idx = frmt.indexOf(EOscanf.PERCENT);
                if (idx == -1) {
                    break;
                }
                final char sym = frmt.charAt(idx + 1);
                if (sym != EOscanf.PERCENT) {
                    output.add(EOscanf.converted(sym, matcher.group(index)));
                    ++index;
                }
                frmt = frmt.substring(idx + 2);
            }
        }
        return new Data.ToPhi(output.toArray(new Phi[0]));
    }

    /**
     * Parse a digits-only match as {@code long} for the {@code %d} conversion,
     * rejecting a match with more digits than {@code long} can hold.
     * @param str Matched digits
     * @return The parsed value
     */
    private static long parsed(final String str) {
        try {
            return Long.parseLong(str);
        } catch (final NumberFormatException ex) {
            throw new ExFailure(
                String.format(
                    "The number %s doesn't fit into long range for the '%%d' conversion",
                    str
                ),
                ex
            );
        }
    }

    /**
     * Convert given string to {@link Phi} depending on format {@code symbol}.
     * @param symbol Format symbol
     * @param str String to format
     * @return Phi object depending on format symbol
     */
    private static Phi converted(final char symbol, final String str) {
        if (!EOscanf.CONVERSION.containsKey(symbol)) {
            throw new ExFailure(
                "The format %c is unsupported, only %s formats can be used",
                symbol, "%s, %d, %f"
            );
        }
        return EOscanf.CONVERSION.get(symbol).apply(str);
    }
}
