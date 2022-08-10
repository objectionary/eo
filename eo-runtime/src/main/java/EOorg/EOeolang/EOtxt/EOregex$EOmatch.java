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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * REGEX.MATCH.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.match")
public class EOregex$EOmatch extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOregex$EOmatch(final Phi sigma) {
        super(sigma);
        this.add("txt", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi regex = rho.attr("ρ").get();
                    final String pattern = new Param(regex, "r").strong(String.class);
                    final String txt = new Param(rho, "txt").strong(String.class);
                    final Matcher matcher = Pattern.compile(pattern).matcher(txt);
                    final List<Phi> dest = new ArrayList<>(0);
                    while (matcher.find()) {
                        final Phi[] groups;
                        if (matcher.groupCount() > 0) {
                            groups = new Phi[matcher.groupCount()];
                            for (int idx = 0; idx < groups.length; idx += 1) {
                                groups[idx] = new Data.ToPhi(matcher.group(idx));
                            }
                        } else {
                            groups = new Phi[] {};
                        }
                        dest.add(
                            new Data.ToPhi(
                                new Phi[] {
                                    new Data.ToPhi(Long.valueOf(matcher.start())),
                                    new Data.ToPhi(matcher.group()),
                                    new Data.ToPhi(groups),
                                }
                            )
                        );
                    }
                    return new Data.ToPhi(dest.toArray(new Phi[] {}));
                }
            )
        );
    }

}
