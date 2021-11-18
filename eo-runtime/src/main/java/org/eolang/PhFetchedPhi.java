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

/**
 * A object with pre-calculuated \phi, which is used when a missing
 * attribute is trying to be taken.
 *
 * @since 0.17
 */
public final class PhFetchedPhi implements Phi {

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
     * Ctor.
     *
     * @param phi The object
     * @param names List of attrs
     */
    public PhFetchedPhi(final Phi base, final Collection<String> names,
        final Phi phi) {
        this.origin = base;
        this.found = phi;
        this.attrs = Collections.unmodifiableCollection(names);
    }

    @Override
    public Phi copy(final Phi rho) {
        return this.origin.copy(rho);
    }

    @Override
    public Attr attr(final int pos) {
        return this.origin.attr(pos);
    }

    @Override
    public Attr attr(final String name) {
        final Attr attr;
        if (this.attrs.contains(name)) {
            attr = this.origin.attr(name);
        } else {
            attr = new AtSimple(this.found);
        }
        return attr;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
