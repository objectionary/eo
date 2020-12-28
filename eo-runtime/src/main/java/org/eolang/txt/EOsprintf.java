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

package org.eolang.txt;

import java.util.Collection;
import java.util.LinkedList;
import org.eolang.AtBound;
import org.eolang.AtDefault;
import org.eolang.AtFree;
import org.eolang.AtVararg;
import org.eolang.Data;
import org.eolang.EObool;
import org.eolang.EOint;
import org.eolang.EOstring;
import org.eolang.Phi;

/**
 * Sprintf.
 *
 * @since 0.2
 */
public class EOsprintf extends Phi {

    public EOsprintf() {
        super();
        this.add("format", new AtFree());
        this.add("args", new AtVararg(new AtFree()));
        this.add("_origin", new AtBound(new AtDefault(() -> {
            final String format = new Data.Take(
                this.attr("format").get()
            ).take(String.class);
            final Phi[] args = new Data.Take(
                this.attr("args").get()
            ).take(Phi[].class);
            final Collection<Object> items = new LinkedList<>();
            for (final Phi arg : args) {
                items.add(EOsprintf.toArg(arg));
            }
            final Phi out = new org.eolang.EOstring();
            out.attr("data").put(
                new Data.Value<>(
                    String.format(format, items.toArray())
                )
            );
            return out;
        })));
    }

    private static Object toArg(final Phi phi) {
        final Object result;
        final Data.Take take = new Data.Take(phi);
        if (phi instanceof EObool) {
            result = take.take(Boolean.class);
        } else if (phi instanceof EOint) {
            result = take.take(Long.class);
        } else if (phi instanceof EOstring) {
            result = take.take(String.class);
        } else {
            throw new UnsupportedOperationException(
                String.format(
                    "Can't print object of type %s",
                    phi.getClass().getName()
                )
            );
        }
        return result;
    }

}
