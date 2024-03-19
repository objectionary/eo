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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A constant object that caches attributes.
 * The object caches only the outer incoming attributes.
 * It means that if attribute is retrieved through other attributes, like {@link Attr#PHI},
 * they won't be cached.
 * {@code
 * 1. [] > x!
 * 2.   y > @
 * 3.   [] > y
 * 4.     "Hello" > z
 * 5. x.z
 * 6. x.z
 * }
 * Here, at the line 6 "x" can't get attribute "z" directly, because it doesn't have it.
 * "x" takes attribute "@" which refers to "y" and then takes "z" from "y".
 * When "x" gets the attribute "z" it caches it.
 * Then, at the line 6 "x" does take attribute "@" again, it just retrieves attribute "y"
 * from its cache.
 * <p>This class is thread-safe.</p>
 *
 * @since 0.16
 */
@Versionized
public final class PhConst implements Phi {

    /**
     * The origin being turned into a const.
     */
    private final Phi origin;

    /**
     * Cached attributes.
     */
    private final Map<String, Attr> cached = new ConcurrentHashMap<>(0);

    /**
     * Ctor.
     * @param phi The origin
     */
    public PhConst(final Phi phi) {
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
        return String.format("!%s%s", this.cached.keySet(), this.origin);
    }

    @Override
    public String φTerm() {
        return String.format("%s!", this.origin.φTerm());
    }

    @Override
    public Phi copy() {
        return new PhConst(this.origin.copy());
    }

    @Override
    public Attr attr(final int pos) {
        return this.origin.attr(pos);
    }

    @Override
    public Attr attr(final String name) {
        final Attr ret;
        if (this.cached.containsKey(name)) {
            ret = this.cached.get(name);
        } else {
            ret = new AtConst(this.origin, this, name);
            this.cached.put(name, ret);
        }
        return ret;
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }
}
