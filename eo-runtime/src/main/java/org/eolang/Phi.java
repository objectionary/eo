/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A simple object.
 *
 * @since 0.1
 */
public class Phi {

    /**
     * Bound attributes.
     */
    private final Map<String, Attr> bound;

    /**
     * Names of free attributes.
     */
    private final List<String> free;

    /**
     * Ctor.
     */
    public Phi(final String... names) {
        this(new HashMap<>(0), new ArrayList<>(Arrays.asList(names)));
    }

    /**
     * Ctor.
     */
    private Phi(final Map<String, Attr> bnd, final List<String> names) {
        this.bound = new HashMap<>(bnd);
        this.free = names;
    }

    /**
     * Make a copy.
     *
     * @return A copy
     */
    public final Phi copy() {
        return new Phi(this.bound, this.free);
    }

    /**
     * Set the attribute.
     *
     * @param name The name of the attribute to set
     * @param phi The value to set
     */
    public final void put(final String name, final Attr phi) {
        if (this.bound.containsKey(name) && !this.free.contains(name)) {
            throw new IllegalStateException(
                String.format(
                    "The attribute \"%s\" is already bound in %s",
                    name, this.getClass().getCanonicalName()
                )
            );
        }
        this.bound.put(name, phi);
    }

    /**
     * Set the attribute by position.
     *
     * @param pos The position of it
     * @param phi The value to set
     */
    public final void put(final int pos, final Attr phi) {
        this.bound.put(this.free.get(pos), phi);
    }

    /**
     * Get the attribute.
     *
     * @param name The name of the attribute to call
     * @return The object
     */
    public final Phi get(final String name) {
        final Attr attr = this.bound.get(name);
        final Phi phi;
        if (attr == null) {
            final Attr origin = this.bound.get("origin");
            if (origin == null) {
                throw new IllegalStateException(
                    String.format(
                        "Can't find \"%s\" attr and there is no origin in %s",
                        name, this.getClass().getCanonicalName()
                    )
                );
            }
            phi = origin.get().get(name);
        } else {
            phi = attr.get();
        }
        return phi;
    }

}
