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

package org.eolang.phi;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A simple object.
 *
 * @since 0.1
 */
public class PhDefault implements Phi {

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
    public PhDefault() {
        this(Phi.ETA);
    }

    /**
     * Ctor.
     *
     * @param prnt Parent
     */
    public PhDefault(final Phi prnt) {
        this(new HashMap<>(0), new ArrayList<>(0));
        this.add(
            "_parent",
            new AtNamed(
                String.format("%s#_parent", this.getClass().getCanonicalName()),
                new AtBound(prnt)
            )
        );
    }

    /**
     * Ctor.
     */
    private PhDefault(final Map<String, Attr> map, final List<String> ordr) {
        this.attrs = map;
        this.order = ordr;
    }

    @Override
    public final String toString() {
        final Collection<String> list = new ArrayList<>(this.attrs.size());
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            list.add(
                String.format(
                    "%s=%s",
                    ent.getKey(),
                    ent.getValue().toString().replace("\n", "\n  ")
                )
            );
        }
        return String.format(
            "%s#%d:{\n  %s\n}",
            this.getClass().getCanonicalName(),
            this.hashCode(),
            String.join("\n  ", list)
        );
    }

    @Override
    public final PhDefault copy() {
        final Map<String, Attr> map = new HashMap<>(this.attrs.size());
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            map.put(ent.getKey(), ent.getValue().copy());
        }
        return new PhDefault(map, this.order);
    }

    @Override
    public final Attr attr(final int pos) {
        if (pos >= this.order.size()) {
            throw new Attr.Exception(
                String.format(
                    "There are just %d attributes, can't find the %dth",
                    this.order.size(), pos
                )
            );
        }
        return this.attr(this.order.get(pos));
    }

    @Override
    public final Attr attr(final String name) {
        Attr attr = this.attrs.get(name);
        if (attr != null) {
            return attr;
        }
        final String[] scopes = {"_origin", "_parent"};
        for (final String scope : scopes) {
            final Attr sub = this.attrs.get(scope);
            if (sub != null) {
                attr = sub.get(this).attr(name);
                if (!(attr instanceof AtAbsent)) {
                    return attr;
                }
            }
        }
        return new AtAbsent(name);
    }

    /**
     * Add new attribute.
     * @param name The name
     * @param attr The attr
     */
    protected final void add(final String name, final Attr attr) {
        if (name.charAt(0) != '_') {
            this.order.add(name);
        }
        this.attrs.put(name, attr);
    }

}
