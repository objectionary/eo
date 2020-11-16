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

package org.eolang.sys;

import java.lang.reflect.Method;

/**
 * Call.
 *
 * @since 0.1
 */
public final class Call implements Phi {

    /**
     * The name of the method to call.
     */
    private final String method;

    /**
     * The object.
     */
    private final Object object;

    /**
     * Arguments to pass.
     */
    private final Args args;

    /**
     * Ctor.
     * @param mtd The method to call
     * @param obj The object
     * @param input Input arguments
     */
    public Call(final String mtd, final Object obj, final Args input) {
        this.method = mtd;
        this.object = obj;
        this.args = input;
    }

    @Override
    public Object call() throws Exception {
        final Method mtd = this.object.getClass().getDeclaredMethod(
            this.method, Args.class
        );
        return mtd.invoke(this.object, this.args);
    }
}
