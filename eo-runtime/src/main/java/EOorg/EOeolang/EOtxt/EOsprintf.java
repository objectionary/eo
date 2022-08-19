/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import java.util.Collection;
import java.util.LinkedList;
import java.util.Locale;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.AtVararg;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sprintf.
 *
 * @since 0.2
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "sprintf")
public class EOsprintf extends PhDefault {

    /**
     * Format flag, replaces a character.
     */
    private static final String CHAR_FLAG = "%c";

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOsprintf(final Phi sigma) {
        super(sigma);
        this.add("format", new AtFree());
        this.add("args", new AtVararg());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final String format = new Param(rho, "format").strong(String.class);
                    final Phi[] args = new Param(rho, "args").strong(Phi[].class);
                    final Collection<Object> items = new LinkedList<>();
                    for (int idx = 0; idx < args.length; idx += 1) {
                        items.add(format(args[idx], idx, format));
                    }
                    return new Data.ToPhi(String.format(Locale.US, format, items.toArray()));
                }
            )
        );
    }

    /**
     * Return formatted object, if argumen is byte array.
     *
     * @param phi Argument of the sprintf
     * @param index Index of the argument in array
     * @param format Format string
     * @return Formatted argument
     * @todo #1049:30min Current implementation supports
     *  only character formatting. It means that byte array
     *  become an first bytes character string. We need to
     *  support another flags, like %a, %o, %s, etc.
     */
    private static Object format(final Phi phi, final int index, final String format) {
        final Object arg = new Dataized(phi).take();
        Object result = arg;
        if (arg instanceof byte[]) {
            final int occurrence = findOccurrence(format, index);
            final String flag = format.substring(occurrence, occurrence + 2);
            if (CHAR_FLAG.equalsIgnoreCase(flag)) {
                result =  (char) ((byte[]) arg)[0];
            }
        }
        return result;
    }

    /**
     * Find index of the flag occurrence.
     *
     * @param format Format string
     * @param index Index of the argument in array
     * @return Index of th flag
     */
    private static int findOccurrence(final String format, final int index) {
        final String flag = "%";
        String fmt = format;
        int result = -1;
        for (int occurrence = 0; occurrence <= index; occurrence += 1) {
            int idx = fmt.indexOf(flag);
            if (idx == -1) {
                result = idx;
                break;
            }
            if (format.charAt(idx + 1) == '%') {
                idx += 1;
            }
            fmt = fmt.substring(idx);
            result = idx;
        }
        return result;
    }
}
