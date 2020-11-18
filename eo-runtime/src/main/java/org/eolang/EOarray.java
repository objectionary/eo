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

package org.eolang;

import org.eolang.sys.Args;
import org.eolang.sys.ArgsException;

/**
 * ARRAY.
 *
 * @since 0.1
 */
public final class EOarray {

    /**
     * Args.
     */
    private final Args args;

    /**
     * Ctor.
     * @param arg Args
     */
    public EOarray(final Args arg) {
        this.args = arg;
    }

    /**
     * The length of it.
     * @param input Input args
     * @return Length
     */
    public Object length(final Args input) {
        int size = 0;
        for (final String key : this.args.keys()) {
            if (key.charAt(0) != '0') {
                continue;
            }
            ++size;
        }
        return size;
    }

    /**
     * Get element by index.
     * @param input Input args
     * @return The object
     * @throws Exception If fails
     */
    public Object get(final Args input) throws Exception {
        final int index = input.call("01", Integer.class);
        final Object result = this.args.call(
            String.format("%02d", index), Object.class
        );
        if (result == null) {
            throw new ArgsException(
                String.format("The item #%d is absent in the array", index)
            );
        }
        return result;
    }
}
