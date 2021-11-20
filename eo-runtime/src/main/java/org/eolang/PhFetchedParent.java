/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * A object with pre-calculuated \phi, which is used when a missing
 * attribute is trying to be taken.
 *
 * @since 0.17
 */
final class PhFetchedParent implements Phi {

    /**
     * Original object.
     */
    private final Phi origin;

    /**
     * Found \phi attribute.
     */
    private final Phi found;

    /**
     * Attributes that definitely exist in the object.
     */
    private final Collection<String> attrs;

    /**
     * TRUE if "found" is no longer trutable.
     */
    private final AtomicBoolean expired;

    /**
     * Ctor.
     *
     * @param base The original object
     * @param names List of attrs
     * @param phi Pref-fetched \phi
     */
    PhFetchedParent(final Phi base, final Collection<String> names,
        final Phi phi) {
        this(base, names, phi, false);
    }

    /**
     * Ctor.
     *
     * @param base The original object
     * @param names List of attrs
     * @param phi Pref-fetched \phi
     * @param exp Expired or not
     */
    private PhFetchedParent(final Phi base, final Collection<String> names,
        final Phi phi, final boolean exp) {
        this.origin = base;
        this.found = phi;
        this.attrs = Collections.unmodifiableCollection(names);
        this.expired = new AtomicBoolean(exp);
    }

    @Override
    public String toString() {
        return String.format(
            "FP%s/%s:%s",
            this.attrs,
            this.found.getClass().getCanonicalName(),
            this.origin.toString()
        );
    }

    @Override
    public Phi copy() {
        return new PhFetchedParent(
            this.origin.copy(),
            this.attrs,
            this.found,
            this.expired.get()
        );
    }

    @Override
    public Phi copy(final Phi rho) {
        return new PhFetchedParent(
            this.origin.copy(rho),
            this.attrs,
            this.found,
            this.expired.get()
        );
    }

    @Override
    public Attr attr(final int pos) {
        return this.origin.attr(pos);
    }

    @Override
    public Attr attr(final String name) {
        final Attr attr;
        if (this.attrs.contains(name) || this.expired.get()) {
            attr = this.origin.attr(name);
        } else {
            attr = this.found.attr(name);
            if ("Δ".equals(name)) {
                this.expired.set(true);
            }
        }
        return attr;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
