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
import org.eolang.Data;

/**
 * Sprintf.
 *
 * @since 0.2
 */
public final class EOsprintf implements Data<String> {

    /**
     * The format.
     */
    private final Data<String> format;

    /**
     * Arguments.
     */
    private final Data<?>[] arguments;

    /**
     * Ctor.
     * @param fmt Format
     * @param args Args
     */
    public EOsprintf(final Data<String> fmt,
        final Data<?>... args) {
        this.format = fmt;
        this.arguments = args;
    }

    @Override
    public String 𝜑() {
        final Collection<Object> items = new LinkedList<>();
        for (final Data<?> arg : this.arguments) {
            items.add(new Data.End(arg).𝜑(Object.class));
        }
        return String.format(
            new Data.End(this.format).𝜑(String.class),
            items.toArray()
        );
    }
}
