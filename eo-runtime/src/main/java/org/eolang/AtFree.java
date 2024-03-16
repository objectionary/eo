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
 * Void attribute.
 *
 * The attribute is not yet set, but can be set. It's writable, but
 * only once.
 *
 * @since 0.1
 */
@Versionized
public final class AtFree implements Attr {
    /**
     * Object that attribute keeps.
     */
    private final AtomicReference<Phi> object;

    /**
     * Ctor.
     */
    public AtFree() {
        this(null);
    }

    /**
     * Ctor for copying.
     * @param phi Object
     */
    private AtFree(final Phi phi) {
        this.object = new AtomicReference<>(phi);
    }

    @Override
    public String toString() {
        return "Void";
    }

    @Override
    public String φTerm() {
//        final String term;
//        if (this.set.get()) {
//            term = this.origin.φTerm();
//        } else {
//            term = "Ø";
//        }
        return "Ø";
    }

    @Override
    public Attr copy(final Phi self) {
        final Phi obj = this.object.get();
        final Phi copy;
        if (obj == null) {
            copy = null;
        } else {
            copy = obj.copy();
        }
        return new AtFree(copy);
    }

    @Override
    public Phi get() {
        final Phi phi = this.object.get();
        if (phi == null) {
            throw new ExUnset(
                "The attribute is not initialized, can't read"
            );
        }
        return phi;
    }

    @Override
    public void put(final Phi phi) {
        if (this.object.get() == null) {
            this.object.set(phi);
        } else {
            throw new ExReadOnly(
                "This void attribute is already set, can't reset"
            );
        }
    }

}
