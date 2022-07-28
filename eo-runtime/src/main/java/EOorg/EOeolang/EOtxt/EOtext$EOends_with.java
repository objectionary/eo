/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2022 Eugene Darashkevich
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

import org.eolang.*;

/**
 * ENDS_WITH.
 *
 * @checkstyle TypeNameCheck (5 lines)
 * @since 1.0
 */
@XmirObject(oname = "text.ends-with")
public class EOtext$EOends_with extends PhDefault {

    /**
     * Ctor.
     *
     * @param sigma Sigma
     */
    public EOtext$EOends_with(final Phi sigma) {
        super(sigma);
        this.add("substr", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final String substring = new Dataized(rho.attr("substr").get()).take(String.class);
                    final Phi text = rho.attr("ρ").get();
                    final String content = new Param(text, "s").strong(String.class);
                    return new Data.ToPhi(content.endsWith(substring));
                }
            )
        );
    }

}
