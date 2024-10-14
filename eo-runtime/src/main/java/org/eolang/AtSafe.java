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
 * The attribute that catches {@link ExFailure} and
 * throws {@link EOerror.ExError}.
 *
 * <p>Every time attribute is taken from an object (when method {@link PhDefault#take(String)}
 * is called) - this attribute is being wrapped by this {@link AtSafe}.
 * It allows to catch {@link ExFailure} inside encapsulated attributes and transform them into
 * {@link EOerror.ExError} which is cached by {@link EOorg.EOeolang.EOtry} lately.</p>
 *
 * @since 0.26
 */
@Versionized
final class AtSafe implements Attr {

    /**
     * Origin attribute.
     */
    private final Attr origin;

    /**
     * Ctor.
     * @param attr Origin attribute
     */
    AtSafe(final Attr attr) {
        this.origin = attr;
    }

    @Override
    public String toString() {
        throw new IllegalStateException(
            "Should never happen"
        );
    }

    @Override
    public String φTerm() {
        throw new IllegalStateException(
            "Should never happen"
        );
    }

    @Override
    public Attr copy(final Phi self) {
        throw new IllegalStateException(
            "Should never happen"
        );
    }

    @Override
    public Phi get() {
        try {
            return new PhSafe(this.origin.get());
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }

    @Override
    public boolean put(final Phi phi) {
        try {
            return this.origin.put(phi);
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }
}
