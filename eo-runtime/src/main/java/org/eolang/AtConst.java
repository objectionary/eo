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
 * Const attribute.
 * When the object is retrieved, it's wrapped with {@link PhFakeRho}.
 * There's no need to wrap instances of {@link Data.Value}. It means that cached attribute
 * was {@link Attr#DELTA}.
 * The wrapping is necessary so that retrieved object doesn't lose information about its
 * "possible" cached \rho ({@link PhConst}).
 * The word "possible" is used, because object may either do not have \rho attribute,
 * or its \rho does not equal to the original parent object wrapped with {@link PhConst}.
 *
 * <p>This class is thread-safe.</p>
 *
 * @since 0.16
 */
@Versionized
final class AtConst implements Attr {

    /**
     * The original object.
     */
    private final Phi origin;

    /**
     * Its possible \rho.
     */
    private final Phi rho;

    /**
     * Cached value of the attribute.
     */
    private final AtomicReference<Phi> cache;

    /**
     * Cached attribute.
     */
    private final Attr attr;

    /**
     * Ctor.
     * @param object The original object
     * @param rho The \rho that possibly will be swapped with \rho of original object
     * @param name The value
     */
    public AtConst(final Phi object, final Phi rho, final String name) {
        this.origin = object;
        this.rho = rho;
        this.attr = new AtFormed(() -> this.origin.attr(name).get());
        this.cache = new AtomicReference<>(null);
    }

    @Override
    public String toString() {
        return String.format("%s!", this.origin.toString());
    }

    @Override
    public String φTerm() {
        return String.format("%s!", this.origin.φTerm());
    }

    @Override
    public Attr copy(final Phi self) {
        throw new IllegalStateException(
            "This should never happen"
        );
    }

    @Override
    public Phi get() {
        synchronized (this.cache) {
            if (this.cache.get() == null) {
                final Phi ret = this.attr.get();
                if (ret instanceof Data.Value) {
                    this.cache.set(ret);
                } else {
                    this.cache.set(
                        new PhFakeRho(ret, this.origin, this.rho)
                    );
                }
            }
        }
        return this.cache.get();
    }

    @Override
    public void put(final Phi src) {
        throw new ExFailure(
            "Constant attribute can't decorate void one"
        );
    }
}
