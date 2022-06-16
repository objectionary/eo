/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

package EOorg.EOeolang.EOtxt;

import EOorg.EOeolang.EOerror;
import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

import java.util.regex.Pattern;

/**
 * REGEX.COMPILE.
 *
 * @since 0.23
 */
@XmirObject(oname = "regex.compile")
public class EOregex$EOcompile extends PhDefault {

    public EOregex$EOcompile(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> {
            final Phi regex = rho.attr("ρ").get();
            final String pattern = new Param(regex, "r").strong(String.class);
            StringBuilder builder = new StringBuilder();
            if (!pattern.startsWith("/")) {
                return new PhWith(
                    new EOerror(Phi.Φ),
                    "msg",
                    new Data.ToPhi("Wrong regex syntax: \"/\" is missing")
                );
            }
            final int lastIndex = pattern.lastIndexOf("/");
            if (!pattern.endsWith("/")) {
                builder.append("(?").append(pattern.substring(lastIndex + 1)).append(")");
            }
            builder.append(pattern, 1, lastIndex);
            Phi phi;
            try {
                String compiled = Pattern.compile(builder.toString()).pattern();
                phi = new PhWith(
                    new EOregex(rho),
                    "r",
                    new Data.ToPhi(compiled)
                );
            } catch (IllegalArgumentException e) {
                phi = new PhWith(
                    new EOerror(Phi.Φ),
                    "msg",
                    new Data.ToPhi(e.getMessage())
                );
            }
            return phi;
        }));
    }

}
