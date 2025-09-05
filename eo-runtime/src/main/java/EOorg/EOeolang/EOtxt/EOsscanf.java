/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOtxt; // NOPMD

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sscanf.
 * @since 0.39
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "sscanf")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOsscanf extends PhDefault implements Atom {
    /**
     * Character conversion.
     */
    private static final Map<Character, Function<String, Phi>> CONVERSION = new HashMap<>();

    static {
        EOsscanf.CONVERSION.put('s', ToPhi::new);
        EOsscanf.CONVERSION.put('d', str -> new Data.ToPhi(Long.parseLong(str)));
        EOsscanf.CONVERSION.put('f', str -> new Data.ToPhi(Double.parseDouble(str)));
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
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOsscanf() {
        this.add("format", new PhVoid("format"));
        this.add("read", new PhVoid("read"));
    }

    @Override
    @SuppressWarnings("PMD.CognitiveComplexity")
    public Phi lambda() {
        final String format = new Dataized(this.take("format")).asString();
        final StringBuilder regex = new StringBuilder();
        boolean literal = false;
        for (int idx = 0; idx < format.length(); ++idx) {
            final char sym = format.charAt(idx);
            if (sym == EOsscanf.PERCENT) {
                if (literal) {
                    regex.append(EOsscanf.PERCENT);
                    literal = false;
                } else {
                    literal = true;
                }
            } else {
                if (literal) {
                    switch (sym) {
                        case 'd':
                            regex.append("(\\d+)");
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
        final Matcher matcher = Pattern.compile(regex.toString()).matcher(
            new Dataized(this.take("read")).asString()
        );
        String frmt = format;
        final List<Phi> output = new ArrayList<>(0);
        if (matcher.find()) {
            int index = 1;
            while (true) {
                final int idx = frmt.indexOf(EOsscanf.PERCENT);
                if (idx == -1) {
                    break;
                }
                final char sym = frmt.charAt(idx + 1);
                if (sym != EOsscanf.PERCENT) {
                    output.add(EOsscanf.converted(sym, matcher.group(index)));
                    ++index;
                }
                frmt = frmt.substring(idx + 2);
            }
        }
        return new Data.ToPhi(output.toArray(new Phi[0]));
    }

    /**
     * Convert given string to {@link Phi} depending on format {@code symbol}.
     * @param symbol Format symbol
     * @param str String to format
     * @return Phi object depending on format symbol
     */
    private static Phi converted(final char symbol, final String str) {
        if (!EOsscanf.CONVERSION.containsKey(symbol)) {
            throw new ExFailure(
                String.format(
                    "The format %c is unsupported, only %s formats can be used",
                    symbol, "%s, %d, %f"
                )
            );
        }
        return EOsscanf.CONVERSION.get(symbol).apply(str);
    }
}
