/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Free attribute with default value.
 *
 * The attribute has default value, but it can be changed.
 * It's writable, but only once.
 *
 * @since 1.0
 * @todo #1087:30m This class duplicates AtFree.
 *  Find a way to replace AtFallback, with a
 *  composition of AtFree & AtSimple. Or
 *  move logic for default value of attribute
 *  entirely to AtFree.
 */
public final class AtFallback implements Attr {
    /**
     * Origin.
     */
    private final Attr origin;

    /**
     * If this is set.
     */
    private final AtomicBoolean set;

    /**
     * Default value.
     */
    private final Attr fallback;

    /**
     * Ctor.
     * @param phi Enclosing phi
     */
    public AtFallback(final Phi phi) {
        this(
            new AtSimple(),
            new AtSimple(phi),
            new AtomicBoolean(false)
        );
    }

    /**
     * Ctor.
     * @param attr Primary attribute.
     * @param flbk Fallback attribute.
     * @param used Is attribute already set.
     */
    public AtFallback(final Attr attr, final Attr flbk, final AtomicBoolean used) {
        this.origin = attr;
        this.fallback = flbk;
        this.set = used;
    }

    @Override
    public String toString() {
        return String.format("%sF", this.origin.toString());
    }

    @Override
    public String φTerm() {
        final String term;
        if (this.set.get()) {
            term = this.origin.φTerm();
        } else {
            term = "Ø";
        }
        return term;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtFallback(
            this.origin.copy(self),
            this.fallback.copy(self),
            new AtomicBoolean(this.set.get())
        );
    }

    @Override
    public Phi get() {
        final Phi result;
        final Phi phi = this.origin.get();
        if (phi.equals(Phi.Φ)) {
            result = this.fallback.get();
        } else {
            result = phi;
        }
        return result;
    }

    @Override
    public void put(final Phi phi) {
        if (this.set.compareAndSet(false, true)) {
            this.origin.put(phi);
        } else {
            throw new ExReadOnly(
                "This free attribute is already set, can't reset"
            );
        }
    }

}
