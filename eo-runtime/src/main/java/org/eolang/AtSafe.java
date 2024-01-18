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
public final class AtSafe implements Attr {

    /**
     * Origin attribute.
     */
    private final Attr origin;

    /**
     * Ctor.
     * @param attr Origin attribute
     */
    public AtSafe(final Attr attr) {
        this.origin = attr;
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
    public Attr copy(final Phi self) {
        return new AtSafe(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        Phi phi;
        try {
            phi = this.origin.get();
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(EOerror.message(ex))
            );
        }
        if (!(phi instanceof Data)) {
            phi = new PhSafe(phi);
        }
        return phi;
    }

    @Override
    public void put(final Phi phi) {
        this.origin.put(phi);
    }

}
