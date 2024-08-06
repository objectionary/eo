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

import EOorg.EOeolang.EOerror;

/**
 * It catches {@link ExFailure} and
 * throws {@link EOerror.ExError}.
 *
 * @since 0.26
 */
@Versionized
@SuppressWarnings("PMD.TooManyMethods")
public final class PhSafe implements Phi {

    /**
     * The original.
     */
    private final Phi origin;

    /**
     * Ctor.
     * @param phi The object
     */
    public PhSafe(final Phi phi) {
        this.origin = phi;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Phi copy() {
        return new PhSafe(this.origin.copy());
    }

    @Override
    public Phi take(final String name) {
        try {
            return this.origin.take(name);
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }

    @Override
    public boolean put(final int pos, final Phi object) {
        try {
            return this.origin.put(pos, object);
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }

    @Override
    public boolean put(final String name, final Phi object) {
        try {
            return this.origin.put(name, object);
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public void attach(final byte[] data) {
        try {
            this.origin.attach(data);
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }

    @Override
    public byte[] delta() {
        try {
            return this.origin.delta();
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }
}
