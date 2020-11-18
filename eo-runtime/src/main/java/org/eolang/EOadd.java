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

import org.cactoos.iterable.Sorted;
import org.eolang.sys.Args;
import org.eolang.sys.ArgsException;
import org.eolang.sys.Phi;
import org.eolang.sys.TypeMismatchException;

/**
 * ADD.
 *
 * @since 0.1
 */
public final class EOadd implements Phi {

    /**
     * Args.
     */
    private final Args args;

    /**
     * Ctor.
     * @param arg Args
     */
    public EOadd(final Args arg) {
        this.args = arg;
    }

    @Override
    @SuppressWarnings("PMD.CyclomaticComplexity")
    public Object call() throws Exception {
        Object result = null;
        for (final String key : new Sorted<>(this.args.keys())) {
            if (key.charAt(0) != '0') {
                continue;
            }
            final Object nxt = this.args.call(key, Object.class);
            if (!(nxt instanceof Long) && !(nxt instanceof Float)) {
                throw new TypeMismatchException(
                    String.format(
                        "Can't use ADD with %s",
                        nxt.getClass()
                    )
                );
            }
            if (result != null && !nxt.getClass().equals(result.getClass())) {
                throw new TypeMismatchException(
                    String.format(
                        "Can't ADD %s to %s",
                        result.getClass(),
                        nxt.getClass()
                    )
                );
            }
            if (result == null) {
                result = nxt;
            } else {
                if (result instanceof Long) {
                    result = Long.class.cast(result) + Long.class.cast(nxt);
                } else {
                    result = Float.class.cast(result) + Float.class.cast(nxt);
                }
            }
        }
        if (result == null) {
            throw new ArgsException(
                "At least one argument required by ADD"
            );
        }
        return result;
    }
}
