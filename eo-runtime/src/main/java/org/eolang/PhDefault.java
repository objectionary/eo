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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

/**
 * A simple object.
 *
 * @since 0.1
 */
public abstract class PhDefault implements Phi, Cloneable {

    /**
     * Attribute name matcher.
     */
    private static final Pattern SORTABLE = Pattern.compile("^[a-z].*$");

    /**
     * Indenter.
     */
    private static final Pattern INDENT = Pattern.compile("\n");

    /**
     * Object recent ID used.
     */
    private static final AtomicInteger IDS = new AtomicInteger();

    /**
     * Shift right.
     */
    private static final String SHIFT = "\n  ";

    /**
     * Identity of it (the ID of the vertex).
     */
    private int vertex;

    /**
     * Attributes.
     */
    private Map<String, Attr> attrs;

    /**
     * Order of their names.
     */
    private final List<String> order;

    /**
     * Cached \phi.
     */
    private final CachedPhi cached = new CachedPhi();

    /**
     * Ctor.
     */
    public PhDefault() {
        this(Phi.Φ);
    }

    /**
     * Ctor.
     *
     * @param sigma Sigma
     */
    public PhDefault(final Phi sigma) {
        this.vertex = PhDefault.IDS.addAndGet(1);
        this.attrs = new HashMap<>(0);
        this.order = new ArrayList<>(0);
        this.add("ρ", new AtSimple(sigma));
        this.add("σ", new AtSimple(sigma));
    }

    @Override
    public boolean equals(final Object obj) {
        return obj instanceof Phi && this.hashCode() == obj.hashCode();
    }

    @Override
    public int hashCode() {
        return this.vertex;
    }

    @Override
    public String φTerm() {
        final Collection<String> list = new ArrayList<>(this.attrs.size());
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            if (!PhDefault.SORTABLE.matcher(ent.getKey()).matches()) {
                continue;
            }
            final String attr = String.format(
                "%s ↦ %s",
                ent.getKey(),
                ent.getValue().φTerm()
            );
            if (attr.endsWith("λ")) {
                continue;
            }
            list.add(attr);
        }
        String txt = this.getClass().getSimpleName();
        final XmirObject xmir = this.getClass().getAnnotation(XmirObject.class);
        if (xmir != null) {
            txt = xmir.oname();
            if ("@".equals(txt)) {
                txt = "φ";
            }
        }
        if (!list.isEmpty()) {
            txt = String.format(
                "%s⟦%s⟧", txt,
                String.join(", ", list)
            );
        }
        return txt;
    }

    @Override
    public String toString() {
        final Collection<String> list = new ArrayList<>(this.attrs.size());
        list.add(String.format("_order=%s", this.order));
        list.add(
            String.format(
                "_cached=%s",
                PhDefault.INDENT.matcher(
                    this.cached.toString()
                ).replaceAll(PhDefault.SHIFT)
            )
        );
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            final int idx = this.order.indexOf(ent.getKey());
            list.add(
                String.format(
                    "%s%s=%s",
                    ent.getKey(),
                    idx >= 0 ? String.format("(#%d)", idx) : "",
                    PhDefault.INDENT.matcher(
                        ent.getValue().toString()
                    ).replaceAll(PhDefault.SHIFT)
                )
            );
        }
        return String.format(
            "%s#%d:{\n  %s\n}",
            this.getClass().getCanonicalName(),
            this.hashCode(),
            String.join(PhDefault.SHIFT, list)
        );
    }

    @Override
    public final Phi copy() {
        return this.copy(this.attrs.get("ρ"));
    }

    @Override
    public final Phi copy(final Phi rho) {
        return this.copy(new AtSimple(rho));
    }

    @Override
    public final Attr attr(final int pos) {
        if (pos < 0) {
            throw new Attr.IllegalAttrException(
                String.format(
                    "Attribute position can't be negative (%d)",
                    pos
                )
            );
        }
        if (this.order.isEmpty()) {
            throw new Attr.IllegalAttrException(
                String.format(
                    "There are no attributes here, can't get the %d-th one",
                    pos
                )
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
            final Attr phi = this.attrs.get("φ");
            if (phi == null) {
                attr = new AtAbsent(
                    name,
                    String.format(
                        " among other %d attrs (%s) and φ is absent",
                        this.attrs.size(),
                        String.join(", ", this.attrs.keySet())
                    )
                );
            } else {
                return new AtSimple(
                    this.cached.get(name, phi::get).attr(name).get().copy(this)
                );
            }
        }
        return this.named(attr, name);
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
        if (PhDefault.SORTABLE.matcher(name).matches()) {
            this.order.add(name);
        }
        this.attrs.put(name, attr);
    }

    /**
     * Make named attribute.
     * @param attr The original attr
     * @param name The name of it
     * @return Named one
     */
    private Attr named(final Attr attr, final String name) {
        return new AtNamed(
            String.format(
                "%s#%s",
                this.getClass().getCanonicalName(), name
            ),
            String.format(
                "%s.%s",
                this.oname(), name
            ),
            this,
            attr
        );
    }

    /**
     * Get its object name, as in source code.
     * @return The name
     */
    private String oname() {
        String txt = this.getClass().getSimpleName();
        final XmirObject xmir = this.getClass().getAnnotation(XmirObject.class);
        if (xmir != null) {
            txt = xmir.oname();
            if ("@".equals(txt)) {
                txt = "φ";
            }
        }
        return txt;
    }

    /**
     * Make a copy with a new \rho.
     * @param rho The rho
     * @return Copy
     */
    private Phi copy(final Attr rho) {
        try {
            final PhDefault copy = PhDefault.class.cast(this.clone());
            copy.vertex = PhDefault.IDS.addAndGet(1);
            final Map<String, Attr> map = new HashMap<>(this.attrs.size());
            for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
                if ("ρ".equals(ent.getKey())) {
                    continue;
                }
                map.put(ent.getKey(), ent.getValue().copy(copy));
            }
            map.put("ρ", rho);
            copy.attrs = map;
            return copy;
        } catch (final CloneNotSupportedException ex) {
            throw new IllegalStateException(ex);
        }
    }

}
