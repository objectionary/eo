/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang.EOtxt;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.StringJoiner;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sprintf.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "sprintf")
public final class EOsprintf extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    public EOsprintf() {
        this.add("format", new AtVoid("format"));
        this.add("args", new AtVoid("args"));
    }

    @Override
    public Phi lambda() throws Exception {
        final String format = new Param(this, "format").strong(String.class);
        final Phi args = this.take("args");
        final Phi retriever = args.take("at");
        final Long length = new Param(args, "length").strong(Long.class);
        final List<Object> arguments = new ArrayList<>(0);
        String pattern = format;
        long index = 0;
        while (true) {
            final int idx = pattern.indexOf('%');
            if (idx == -1) {
                break;
            }
            if (index == length) {
                throw new ExFailure(
                    String.format(
                        "The amount of arguments %d does not match the amount of format occurrences %d",
                        length,
                        EOsprintf.percents(format)
                    )
                );
            }
            final char sym = pattern.charAt(idx + 1);
            final Phi taken = retriever.copy();
            taken.put(0, new Data.ToPhi(index));
            arguments.add(EOsprintf.formatted(sym, new Dataized(taken)));
            pattern = pattern.substring(idx + 1);
            ++index;
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
        final Object result;
        switch (symbol) {
            case 's':
                result = element.take(String.class);
                break;
            case 'd':
                result = element.take(Long.class);
                break;
            case 'f':
                result = element.take(Double.class);
                break;
            case 'x':
                result = EOsprintf.bytesToHex(element.take());
                break;
            case 'b':
                result = element.take(Boolean.class);
                break;
            default:
                throw new ExFailure(
                    String.format(
                        "The format %c is unsupported, only %s formats can be used",
                        symbol,
                        "%s, %d, %f, %x, %b"
                    )
                );
        }
        return result;
    }

    /**
     * Count amount of '%' char occurrences.
     * @param str Given string
     * @return Amount of '%' in string
     */
    private static int percents(final String str) {
        int count = 0;
        for (int idx = 0; idx < str.length(); ++idx) {
            if (str.charAt(idx) == '%') {
                ++count;
            }
        }
        return count;
    }
}
