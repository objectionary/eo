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

package EOorg.EOeolang;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * AS-REGEX.
 *
 */
@XmirObject(oname = "string.as-regex")
public class EOstring$EOas_regex extends PhDefault {

    public EOstring$EOas_regex(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> {
            final String pattern = new Param(rho).strong(String.class);
            StringBuilder builder = new StringBuilder();
            if (pattern.startsWith("/")) {
                if (!pattern.endsWith("/")) {
                    builder.append("(?").append(pattern.substring(pattern.length() - 1)).append(")");
                }
                builder.append(pattern, 1, pattern.length() - 2);
                return new Data.ToPhi(Pattern.compile(builder.toString()));
            } else {
                throw new PatternSyntaxException("Wrong regex syntax", pattern, 0);
            }
        }
        ));
    }

}
