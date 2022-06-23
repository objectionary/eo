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

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * sscanf.
 *
 * @since 0.23
 */
@XmirObject(oname = "sscanf")
public class EOsscanf extends PhDefault {

    public EOsscanf(final Phi sigma) {
        super(sigma);
        this.add("format", new AtFree());
        this.add("read", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final String format = new Param(rho, "format").strong(String.class);
            final String read = new Param(rho, "read").strong(String.class);
            final List<Phi> buffer = new ArrayList<>();
            Scanner fsc = new Scanner(format);
            Scanner rsc = new Scanner(read);
            while (fsc.hasNext() && rsc.hasNext()) {
                String next = fsc.next();
                String val = rsc.next();
                if (next.startsWith("%")) {
                    if (next.charAt(1) == 's') {
                        buffer.add(new Data.ToPhi(val));
                    } else if (next.charAt(1) == 'd') {
                        buffer.add(new Data.ToPhi(Long.parseLong(val)));
                    } else if (next.charAt(1) == 'f') {
                        buffer.add(new Data.ToPhi(Double.parseDouble(val)));
                    }
                }
            }

            return new Data.ToPhi(buffer.toArray(new Phi[0]));
        }));
    }

}
