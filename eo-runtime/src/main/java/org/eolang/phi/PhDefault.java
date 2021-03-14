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
public class PhDefault implements Phi, Cloneable {

    /**
     * Attributes.
     */
    private Map<String, Attr> attrs;

    /**
     * Order of their names.
     */
    private final List<String> order;

    /**
     * Ctor.
     */
    public PhDefault() {
        this(new PhEta());
    }

    /**
     * Ctor.
     *
     * @param prnt Parent
     */
    public PhDefault(final Phi prnt) {
        this.attrs = new HashMap<>(0);
        this.order = new ArrayList<>(0);
        this.add("ρ", new AtSimple(prnt));
    }

    @Override
    public String toString() {
        final Collection<String> list = new ArrayList<>(this.attrs.size());
        list.add(
            String.format(
                "_order=%s",
                this.order
            )
        );
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            final int idx = this.order.indexOf(ent.getKey());
            list.add(
                String.format(
                    "%s%s=%s",
                    ent.getKey(),
                    idx >=0 ? String.format("(%d)", idx) : "",
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
    public final Phi copy() {
        try {
            final PhDefault copy = PhDefault.class.cast(this.clone());
            final Map<String, Attr> map = new HashMap<>(this.attrs.size());
            for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
                map.put(ent.getKey(), ent.getValue().copy(copy));
            }
            copy.attrs = map;
            return copy;
        } catch (final CloneNotSupportedException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public final Attr attr(final int pos) {
        if (this.order.isEmpty()) {
            throw new Attr.Exception(
                "There are no attributes here"
            );
        }
        final int idx;
        if (pos >= this.order.size()) {
            idx = this.order.size() - 1;
        } else {
            idx = pos;
        }
        return this.attr(this.order.get(idx));
    }

    @Override
    public final Attr attr(final String name) {
        Attr attr = this.attrs.get(name);
        if (attr == null) {
            final Attr sub = this.attrs.get("φ");
            if (sub == null) {
                attr = new AtNamed(
                    String.format(
                        "%s#%s",
                        this.getClass().getCanonicalName(),
                        name
                    ),
                    new AtAbsent(name)
                );
            } else {
                attr = sub.get().attr(name);
                if (!(attr instanceof AtAbsent)) {
                    attr = new AtChild(attr);
                }
            }
        }
        return attr;
    }

    /**
     * Add new attribute.
     *
     * This method can only be called from child classes, in their
     * constructors, when the declare their attributes. This is why it's
     * protected. Not the brightest design, I admit.
     *
     * @param name The name
     * @param attr The attr
     */
    protected final void add(final String name, final Attr attr) {
        if (name.matches("^[a-z0-9]+$")) {
            this.order.add(name);
        }
        this.attrs.put(
            name,
            new AtNamed(
                String.format(
                    "%s#%s",
                    this.getClass().getCanonicalName(),
                    name
                ),
                attr
            )
        );
    }

    private final class AtChild implements Attr {
        private final Attr origin;
        AtChild(final Attr attr) {
            this.origin = attr;
        }
        @Override
        public Attr copy(final Phi self) {
            return new AtChild(this.origin.copy(self));
        }
        @Override
        public Phi get() {
            final Phi phi = this.origin.get();
            if (phi instanceof Data) {
                return phi;
            }
            phi.attr("ρ").put(PhDefault.this);
            return phi;
        }
        @Override
        public void put(final Phi phi) {
            this.origin.put(phi);
        }
    }
}
