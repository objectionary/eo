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

import org.eolang.sys.EObool;
import org.eolang.sys.Phi;
import org.eolang.sys.Primitive;

/**
 * AND.
 *
 * @since 0.1
 */
public final class EOand implements Phi {

    /**
     * Args.
     */
    private final Phi[] args;

    /**
     * Ctor.
     * @param items The items
     */
    public EOand(final Phi... items) {
        this.args = items;
    }

    @Override
    public Phi 𝜑() {
        boolean result = true;
        for (final Phi arg : this.args) {
            if (!new Primitive.End(arg).take(EObool.class).data()) {
                result = false;
                break;
            }
        }
        return new EObool(result);
    }

}
