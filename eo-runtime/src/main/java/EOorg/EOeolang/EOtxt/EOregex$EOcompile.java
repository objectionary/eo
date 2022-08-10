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

import java.util.regex.Pattern;
import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * REGEX.COMPILE.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.compile")
public class EOregex$EOcompile extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOregex$EOcompile(final Phi sigma) {
        super(sigma);
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi regex = rho.attr("ρ").get();
                    final String pattern = new Param(regex, "r").strong(String.class);
                    final StringBuilder builder = new StringBuilder();
                    if (!pattern.startsWith("/")) {
                        throw new ExFailure("Wrong regex syntax: \"/\" is missing");
                    }
                    final int last = pattern.lastIndexOf("/");
                    if (!pattern.endsWith("/")) {
                        builder.append("(?").append(pattern.substring(last + 1)).append(")");
                    }
                    builder.append(pattern, 1, last);
                    try {
                        final String compiled = Pattern.compile(builder.toString()).pattern();
                        return new PhWith(
                            new EOregex(rho),
                            "r",
                            new Data.ToPhi(compiled)
                        );
                    } catch (final IllegalArgumentException ex) {
                        throw new ExFailure(ex.getMessage());
                    }
                }
            )
        );
    }

}
