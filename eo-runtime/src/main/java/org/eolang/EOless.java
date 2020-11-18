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
import org.eolang.sys.Phi;
import org.eolang.sys.TypeMismatchException;

/**
 * LESS.
 *
 * @since 0.2
 */
public final class EOless implements Phi {

    /**
     * Args.
     */
    private final Args args;

    /**
     * Ctor.
     * @param arg Args
     */
    public EOless(final Args arg) {
        this.args = arg;
    }

    @Override
    public Object call() throws Exception {
        final Object left = this.args.call("01", Number.class);
        final Object right = this.args.call("02", Number.class);
        final boolean result;
        if (left instanceof Long && right instanceof Long) {
            result = Long.class.cast(left).compareTo(
                Long.class.cast(right)
            ) < 0;
        } else if (left instanceof Float && right instanceof Float) {
            result = Float.class.cast(left).compareTo(
                Float.class.cast(right)
            ) < 0;
        } else {
            throw new TypeMismatchException(
                String.format(
                    "Can't compare %s with %s",
                    left.getClass(),
                    right.getClass()
                )
            );
        }
        return result;
    }
}
