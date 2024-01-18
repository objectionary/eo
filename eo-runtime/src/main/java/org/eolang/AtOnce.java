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

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * Read only once.
 *
 * @since 0.1
 */
@Versionized
public final class AtOnce implements Attr {

    /**
     * Origin attribute.
     */
    private final Attr origin;

    /**
     * Cache.
     */
    private final AtomicReference<Phi> cached;

    /**
     * Ctor.
     * @param attr Origin attribute
     */
    public AtOnce(final Attr attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>();
    }

    @Override
    public String toString() {
        final String txt;
        final Phi phi = this.cached.get();
        if (phi == null) {
            txt = String.format("%sO", this.origin.toString());
        } else {
            txt = phi.toString();
        }
        return txt;
    }

    @Override
    public String φTerm() {
        final String txt;
        final Phi phi = this.cached.get();
        if (phi == null) {
            txt = "λ";
        } else {
            txt = phi.φTerm();
        }
        return txt;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtOnce(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        synchronized (this.cached) {
            if (this.cached.get() == null) {
                this.cached.set(this.origin.get());
            }
        }
        return this.cached.get();
    }

    @Override
    public void put(final Phi phi) {
        throw new ExReadOnly(
            String.format(
                "You can't overwrite '%s'",
                this.origin
            )
        );
    }

}
