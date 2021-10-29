/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import org.eolang.AtBound;
import org.eolang.AtFree;
import org.eolang.AtLambda;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * JOIN.
 *
 * @since 1.0
 */
public class EOstring$EOjoined extends PhDefault {

    public EOstring$EOjoined(final Phi parent, final EOstring up) {
        super(parent);
        this.add("items", new AtFree());
        this.add("φ", new AtBound(new AtLambda(this, self -> {
            final String delim = new Dataized(
                self.attr("ρ").get()
            ).take(String.class);
            final Phi[] items = new Dataized(
                self.attr("items").get()
            ).take(Phi[].class);
            final String[] texts = new String[items.length];
            for (int idx = 0; idx < texts.length; ++idx) {
                texts[idx] = new Dataized(items[idx]).take(String.class);
            }
            return new Data.ToPhi(String.join(delim, texts));
        })));
    }

}
