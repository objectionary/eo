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
import java.util.Locale;
import java.util.Map;
import java.util.StringJoiner;
import java.util.function.Function;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sprintf.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "sprintf")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOsprintf extends PhDefault implements Atom {
    /**
     * Character conversion.
     */
    private static final Map<Character, Function<Dataized, Object>> CONVERSION = new HashMap<>();

    static {
        EOsprintf.CONVERSION.put('s', Dataized::asString);
        EOsprintf.CONVERSION.put('d', element -> element.asNumber().longValue());
        EOsprintf.CONVERSION.put('f', Dataized::asNumber);
        EOsprintf.CONVERSION.put('x', element -> EOsprintf.bytesToHex(element.take()));
        EOsprintf.CONVERSION.put('b', Dataized::asBool);
    }

    /**
     * Percent sign.
     */
    private static final char PERCENT = '%';

    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOsprintf() {
        this.add("format", new AtVoid("format"));
        this.add("args", new AtVoid("args"));
    }

    @Override
    public Phi lambda() {
        final String format = new Dataized(this.take("format")).asString();
        final Phi retriever = Expect.at(this, "args")
            .that(phi -> phi.take("at"))
            .otherwise("be a tuple with the 'at' attribute")
            .it();
        final long length = Expect.at(this, "args")
            .that(phi -> new Dataized(phi.take("length")).asNumber().intValue())
            .otherwise("be a tuple with the 'length' attribute")
            .it();
        final List<Object> arguments = new ArrayList<>(0);
        String pattern = format;
        long index = 0;
        while (true) {
            final int idx = pattern.indexOf(EOsprintf.PERCENT);
            if (idx == -1) {
                break;
            }
            if (index == length) {
                throw new ExFailure(
                    String.format(
                        "The amount of arguments %d does not match the amount of format occurrences %d",
                        length,
                        EOsprintf.formats(format)
                    )
                );
            }
            final char sym = pattern.charAt(idx + 1);
            if (sym == EOsprintf.PERCENT) {
                pattern = pattern.substring(idx + 1);
            } else {
                final Phi taken = retriever.copy();
                taken.put(0, new Data.ToPhi(index));
                arguments.add(EOsprintf.formatted(sym, new Dataized(taken)));
                ++index;
                pattern = pattern.substring(idx + 2);
            }
        }
        return new ToPhi(
            String.format(
                Locale.US,
                format.replaceAll("%x", "%s"),
                arguments.toArray()
            )
        );
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

    /**
     * Format given {@code element} depending on format char.
     * @param symbol Format char
     * @param element Element ready for formatting
     * @return Formatted object
     */
    private static Object formatted(final char symbol, final Dataized element) {
        if (!EOsprintf.CONVERSION.containsKey(symbol)) {
            throw new ExFailure(
                String.format(
                    "The format %c is unsupported, only %s formats can be used",
                    symbol, "%s, %d, %f, %x, %b"
                )
            );
        }
        return EOsprintf.CONVERSION.get(symbol).apply(element);
    }

    /**
     * Count amount of format occurrences.
     * @param str Given string
     * @return Amount of formats in string
     */
    private static int formats(final String str) {
        int count = 0;
        for (int idx = 0; idx < str.length(); ++idx) {
            if (
                str.charAt(idx) == EOsprintf.PERCENT
                    && idx + 1 != str.length()
                    && EOsprintf.CONVERSION.containsKey(str.charAt(idx + 1))
            ) {
                ++count;
            }
        }
        return count;
    }
}
