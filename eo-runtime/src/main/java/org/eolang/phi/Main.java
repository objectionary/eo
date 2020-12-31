/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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

package org.eolang.phi;

import org.eolang.EOstring;

/**
 * Bridge between Java CLI and EO.
 *
 * @since 0.1
 */
public final class Main {

    public static void main(final String... args) throws Exception {
        final String path = args[0].replaceAll("([^\\.]+)$", "EO$1");
        final Phi app = Phi.class.cast(
            Class.forName(path).getConstructor().newInstance()
        );
        for (int idx = 1; idx < args.length; ++idx) {
            final Phi phi = new EOstring();
            final String arg = args[idx];
            phi.attr("data").put(new Data.Value<>(arg));
            app.attr("args").put(phi);
        }
        if (!new Data.Take(app).take(Boolean.class)) {
            throw new IllegalStateException(
                "Runtime failure"
            );
        }
    }

}
