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
     * ETA.
     */
    public static final Phi ETA = new Phi();

    /**
     * Attributes.
     */
    private final Map<String, Attr> attrs;

    /**
     * Order of their names.
     */
    private final List<String> order;

    /**
     * Ctor.
     */
    public Phi() {
        this(new HashMap<>(0), new ArrayList<>(0));
    }

    /**
     * Ctor.
     */
    private Phi(final Map<String, Attr> map, final List<String> ordr) {
        this.attrs = map;
        this.order = ordr;
    }

    /**
     * Add new attribute.
     * @param name The name
     * @param attr The attr
     */
    protected final void add(final String name, final Attr attr) {
        this.order.add(name);
        this.attrs.put(name, attr);
    }

    /**
     * Make a copy.
     *
     * @return A copy
     */
    public final Phi copy() {
        final Map<String, Attr> map = new HashMap<>(this.attrs.size());
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            map.put(ent.getKey(), ent.getValue().copy());
        }
        return new Phi(map, this.order);
    }

    /**
     * Get attribute by position.
     *
     * @param pos The position of the attribute
     * @return The attr
     */
    public final Attr attr(final int pos) {
        return this.attr(this.order.get(pos));
    }

    /**
     * Get attribute.
     *
     * @param name The name of the attribute
     * @return The attr
     */
    public final Attr attr(final String name) {
        Attr attr = this.attrs.get(name);
        if (attr == null) {
            final Attr origin = this.attrs.get("_origin");
            if (origin == null) {
                throw new IllegalStateException(
                    String.format(
                        "Can't find \"%s\" attr among (%s) and there is no origin in %s",
                        name,
                        String.join(
                            ", ",
                            this.attrs.keySet()
                        ),
                        this.getClass().getCanonicalName()
                    )
                );
            }
            attr = origin.get().attr(name);
        }
        return attr;
    }

    /**
     * Inherit attributes from parent object.
     *
     * @param parent The parent
     */
    public final Phi inherit(final Phi parent) {
        this.attr("_parent").put(parent);
        return this;
    }

}
