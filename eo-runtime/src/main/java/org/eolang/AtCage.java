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

import EOorg.EOeolang.EOcage;

/**
 * An attribute that knows how to store an object inside {@link EOcage}.
 *
 * @since 0.29.6
 */
@Versionized
public final class AtCage implements Attr {

    /**
     * The term to show when empty.
     */
    private static final String EMPTY_TERM = "Ø";

    /**
     * The object.
     */
    private Phi object;

    /**
     * Ctor.
     */
    public AtCage() {
        this(null);
    }

    /**
     * Ctor for copying.
     * @param obj New object.
     */
    private AtCage(final Phi obj) {
        this.object = obj;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtCage(this.object);
    }

    @Override
    public Phi get() {
        if (this.object == null) {
            throw new ExFailure(
                "The cage is empty, can't read it"
            );
        }
        return this.object;
    }

    @Override
    public void put(final Phi phi) {
        if (this.object != null && !this.object.forma().equals(phi.forma())) {
            throw new ExFailure(
                "Can't write an object formed by %s because object formed by %s was saved before",
                phi.forma(),
                this.object.forma()
            );
        }
        this.object = phi;
    }

    @Override
    public String φTerm() {
        final String txt;
        if (this.object == null) {
            txt = AtCage.EMPTY_TERM;
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
