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
 * A kid object found in \phi.
 *
 * @since 0.17
 */
final class PhFetchedKids implements Phi {

    /**
     * The original.
     */
    private final Phi origin;

    /**
     * List of present attributes in the parent, where this one
     * was found.
     */
    private final Collection<String> attrs;

    /**
     * Ctor.
     *
     * @param phi The object
     * @param names List of attrs
     */
    PhFetchedKids(final Phi phi, final Collection<String> names) {
        this.origin = phi;
        this.attrs = Collections.unmodifiableCollection(names);
    }

    @Override
    public String toString() {
        return String.format(
            "FK%s:%s",
            this.attrs, this.origin.toString()
        );
    }

    @Override
    public Phi copy() {
        return new PhFetchedKids(this.origin.copy(), this.attrs);
    }

    @Override
    public Phi copy(final Phi rho) {
        return new PhFetchedKids(this.origin.copy(rho), this.attrs);
    }

    @Override
    public Attr attr(final int pos) {
        return this.origin.attr(pos);
    }

    @Override
    public Attr attr(final String name) {
        return new AtComposite(
            this.origin, rho -> {
                Phi phi = this.origin.attr(name).copy(rho).get();
                if ("ρ".equals(name)) {
                    phi = new PhFetchedParent(phi, this.attrs, this.origin);
                }
                return phi;
            }
        );
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
