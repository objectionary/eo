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
import org.eolang.sys.EOstring;
import org.eolang.sys.Phi;
import org.eolang.sys.Primitive;

/**
 * Sprintf.
 *
 * @since 0.2
 */
public final class EOsprintf implements Phi {

    /**
     * The format.
     */
    private final Phi format;

    /**
     * Arguments.
     */
    private final Phi[] arguments;

    /**
     * Ctor.
     * @param fmt Format
     * @param args Args
     */
    public EOsprintf(final Phi fmt, final Phi... args) {
        this.format = fmt;
        this.arguments = args;
    }

    @Override
    public Phi 𝜑() {
        final Collection<Object> items = new LinkedList<>();
        for (final Phi arg : this.arguments) {
            items.add(new Primitive.End(arg).take(EOstring.class).data());
        }
        return new EOstring(
            String.format(
                new Primitive.End(this.format).take(EOstring.class).data(),
                items.toArray()
            )
        );
    }
}
