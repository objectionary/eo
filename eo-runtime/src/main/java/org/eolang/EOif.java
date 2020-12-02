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
 * IF.
 *
 * @since 0.2
 */
public final class EOif implements Phi {

    /**
     * The condition.
     */
    private final Phi term;

    /**
     * When TRUE.
     */
    private final Phi positive;

    /**
     * When FALSE.
     */
    private final Phi negative;

    /**
     * Ctor.
     * @param trm The term
     * @param pos The positive
     * @param neg The negative
     */
    public EOif(final Phi trm, final Phi pos, final Phi neg) {
        this.term = trm;
        this.positive = pos;
        this.negative = neg;
    }

    @Override
    public Phi 𝜑() {
        final boolean yes = new Primitive.End(this.term)
            .take(EObool.class).data();
        final Phi result;
        if (yes) {
            result = this.positive;
        } else {
            result = this.negative;
        }
        return result;
    }
}
