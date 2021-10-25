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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A const object.
 *
 * @since 0.16
 */
public final class PhConst implements Phi {

    /**
     * The object fetched.
     */
    private final Phi object;

    /**
     * Cached attributes.
     */
    private final Map<String, Attr> named;

    /**
     * Cached attributes.
     */
    private final Map<Integer, Attr> numbered;

    /**
     * Ctor.
     *
     * @param phi The object
     */
    public PhConst(final Phi phi) {
        this(phi, new ConcurrentHashMap<>(0), new ConcurrentHashMap<>(0));
    }

    /**
     * Ctor.
     *
     * @param phi The object
     */
    public PhConst(final Phi phi, final Map<String, Attr> names,
        final Map<Integer, Attr> nums) {
        this.object = phi;
        this.named = names;
        this.numbered = nums;
    }

    @Override
    public String toString() {
        return String.format("!%s", this.object.toString());
    }

    @Override
    public Phi copy() {
        return new PhConst(this.object.copy(), this.named, this.numbered);
    }

    @Override
    public Attr attr(final int pos) {
        this.numbered.computeIfAbsent(
            pos, x -> new AtConst(this.object.attr(pos).copy(this))
        );
        return this.numbered.get(pos);
    }

    @Override
    public Attr attr(final String name) {
        this.named.computeIfAbsent(
            name, x -> new AtConst(this.object.attr(name).copy(this))
        );
        return this.named.get(name);
    }
}
