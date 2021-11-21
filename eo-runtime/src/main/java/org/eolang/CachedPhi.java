/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Supplier;

/**
 * Cached \phi.
 *
 * @since 0.17
 */
final class CachedPhi {

    /**
     * Cached \phi.
     */
    private final ConcurrentMap<Integer, Phi> cached = new ConcurrentHashMap<>(1);

    @Override
    public String toString() {
        final Phi phi = this.cached.get(0);
        final String txt;
        if (phi == null) {
            txt = "NULL";
        } else {
            txt = phi.toString();
        }
        return txt;
    }

    /**
     * Get it and clean if necessary.
     * @param name Attribute name
     * @param supplier of Phi
     */
    public Phi get(final String name, final Supplier<Phi> supplier) {
        this.cached.computeIfAbsent(0, x -> supplier.get());
        final Phi ret = this.cached.get(0);
        if ("Δ".equals(name)) {
            this.cached.remove(0);
        }
        return ret;
    }

}
