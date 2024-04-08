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
 * Object that eliminates duplication of data calculation.
 * When this object is accessed for an attribute, it:
 * 1. dataizes the encapsulated one
 * 2. makes new instance of {@link EOorg.EOeolang.EObytes} with given data (bytes)
 * 3. caches it
 * 4. behaves as cached {@link EOorg.EOeolang.EObytes}
 * @since 0.16
 */
@Versionized
public final class PhConst implements Phi {

    /**
     * The object being wrapped by const decorator.
     */
    private final Phi wrapped;

    /**
     * Bytes retrieved from the wrapped object.
     */
    private final AtomicReference<Phi> bytes;

    /**
     * Ctor.
     * @param phi The origin wrapped phi
     */
    public PhConst(final Phi phi) {
        this(phi, null);
    }

    /**
     * Ctor for copying.
     * @param phi The origin wrapped phi
     * @param bts EObytes
     */
    private PhConst(final Phi phi, final Phi bts) {
        this.wrapped = phi;
        this.bytes = new AtomicReference<>(bts);
    }

    @Override
    public boolean equals(final Object obj) {
        return this.wrapped.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.wrapped.hashCode();
    }

    @Override
    public String toString() {
        return String.format("!%s", this.wrapped);
    }

    @Override
    public String φTerm() {
        return String.format("%s!", this.wrapped.φTerm());
    }

    @Override
    public Phi copy() {
        return new PhConst(this.wrapped.copy(), this.primitive());
    }

    @Override
    public Phi take(final String name) {
        return this.primitive().take(name);
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.primitive().put(pos, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.primitive().put(name, object);
    }

    @Override
    public String locator() {
        return this.wrapped.locator();
    }

    @Override
    public String forma() {
        return this.wrapped.forma();
    }

    @Override
    public void attach(final byte[] data) {
        this.primitive().attach(data);
    }

    @Override
    public byte[] delta() {
        return this.primitive().delta();
    }

    /**
     * Cached primitive object that was retrieved from dataization of wrapped one.
     * @return EObytes object
     */
    private Phi primitive() {
        synchronized (this.bytes) {
            if (this.bytes.get() == null) {
                this.bytes.set(
                    new Data.ToPhi(
                        new Dataized(this.wrapped).take()
                    )
                );
            }
        }
        return this.bytes.get();
    }
}
