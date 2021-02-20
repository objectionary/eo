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

package org.eolang.io;

import org.eolang.phi.AtBound;
import org.eolang.phi.AtLambda;
import org.eolang.phi.Data;
import org.eolang.phi.PhDefault;
import org.eolang.phi.PhWith;
import org.eolang.phi.Phi;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * Standard Input. Consumes all data.
 *
 * @since 0.1
 */
public class EOstdin$EOφ extends PhDefault {

    public EOstdin$EOφ(final Phi parent) {
        super(parent);
        this.add("φ",new AtBound(new AtLambda(this, self -> {
            try (BufferedInputStream bis = new BufferedInputStream(System.in);
                 ByteArrayOutputStream buf = new ByteArrayOutputStream()) {
                int result = bis.read();
                while(result != -1) {
                    buf.write((byte) result);
                    result = bis.read();
                }
                return new Data.ToPhi(buf.toString(StandardCharsets.UTF_8.name()));
            }
            catch(IOException e) {
                final Phi msg = new Data.ToPhi("Cannot read from the standard input stream: " + e.getMessage());
                return new PhWith(new org.eolang.EOerror(), "msg", msg);
            }
        })));
    }

}
