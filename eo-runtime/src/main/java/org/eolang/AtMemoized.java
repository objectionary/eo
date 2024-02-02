/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package org.eolang;

/**
 * An attribute that knows how to memoize an object.
 *
 * @since 0.24
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
public final class AtMemoized implements Attr {

    /**
     * The term to show when empty.
     */
    public static final String EMPTY_TERM = "Ø";

    /**
     * The object in memory.
     */
    private Phi object;

    /**
     * Allocated bytes length.
     */
    private Integer length;

    /**
     * Ctor, needed for copying.
     */
    public AtMemoized() {
        this(null, null);
    }

    /**
     * Ctor, needed for copying.
     * @param obj New object
     * @param size New size
     */
    private AtMemoized(final Phi obj, final Integer size) {
        this.object = obj;
        this.length = size;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtMemoized(this.object, this.length);
    }

    @Override
    public Phi get() {
        if (this.object == null) {
            throw new ExFailure(
                "The memory is empty, can't read it"
            );
        }
        return this.object;
    }

    @Override
    public void put(final Phi phi) {
        final byte[] bytes = new Param(phi, "Δ").asBytes().take();
        if (this.length == null) {
            this.length = bytes.length;
        } else if (this.length < bytes.length) {
            throw new ExFailure(
                "Can't write to memory %d bytes because %d were already allocated",
                bytes.length,
                this.length
            );
        }
        this.object = new Data.ToPhi(bytes);
    }

    @Override
    public String φTerm() {
        final String txt;
        if (this.object == null) {
            txt = AtMemoized.EMPTY_TERM;
        } else {
            txt = this.object.φTerm();
        }
        return txt;
    }

    @Override
    public String toString() {
        final String txt;
        if (this.object == null) {
            txt = "NULL";
        } else {
            txt = this.object.toString();
        }
        return String.format("%d->%s", this.hashCode(), txt);
    }

}
