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
        return PhSafe.through(() -> this.origin.equals(obj));
    }

    @Override
    public int hashCode() {
        return PhSafe.through(this.origin::hashCode);
    }

    @Override
    public String toString() {
        return PhSafe.through(this.origin::toString);
    }

    @Override
    public String φTerm() {
        return PhSafe.through(this.origin::φTerm);
    }

    @Override
    public Phi copy() {
        return PhSafe.through(() -> new PhSafe(this.origin.copy()));
    }

    @Override
    public Phi take(final String name) {
        return PhSafe.through(() -> new PhSafe(this.origin.take(name)));
    }

    @Override
    public boolean put(final int pos, final Phi object) {
        return PhSafe.through(() -> this.origin.put(pos, object));
    }

    @Override
    public boolean put(final String name, final Phi object) {
        return PhSafe.through(() -> this.origin.put(name, object));
    }

    @Override
    public String locator() {
        return PhSafe.through(this.origin::locator);
    }

    @Override
    public String forma() {
        return PhSafe.through(this.origin::forma);
    }

    @Override
    public void attach(final byte[] data) {
        PhSafe.through(
            () -> {
                this.origin.attach(data);
                return null;
            }
        );
    }

    @Override
    public byte[] delta() {
        return PhSafe.through(this.origin::delta);
    }

    /**
     * Helper, for other methods.
     * @param action The action
     * @param <T> Type of result
     * @return Result
     */
    private static <T> T through(final Action<T> action) {
        try {
            return action.act();
        } catch (final ExFailure ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(new EOerror.ErrorMsg(ex).get())
            );
        }
    }

}
