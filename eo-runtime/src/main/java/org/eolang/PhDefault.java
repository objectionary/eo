/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * A simple object.
 *
 * The class is thread-safe.
 *
 * @since 0.1
 */
public abstract class PhDefault implements Phi, Cloneable {

    /**
     * Attribute name matcher.
     */
    private static final Pattern SORTABLE = Pattern.compile("^[a-z].*$");

    /**
     * Vertices.
     */
    protected static final Vertices VTX = new Vertices();

    /**
     * Order of their names.
     */
    private final List<String> order;

    /**
     * Identity of it (the ID of the vertex).
     */
    protected int vertex;

    /**
     * Attributes.
     */
    private Map<String, Attr> attrs;

    /**
     * Cached \phi.
     */
    private CachedPhi cached = new CachedPhi();

    /**
     * Terms being processed now.
     */
    private final ThreadLocal<Set<Integer>> terms = new ThreadLocal<>();

    /**
     * Terms being processed now.
     */
    private final ThreadLocal<Set<Integer>> strings = new ThreadLocal<>();

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
        this.vertex = PhDefault.VTX.next();
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
        if (this.terms.get() == null) {
            this.terms.set(new HashSet<>());
        }
        if (this.terms.get().contains(this.vertex)) {
            return String.format("ν%d", this.vertex);
        }
        this.terms.get().add(this.vertex);
        final List<String> list = new ArrayList<>(this.attrs.size());
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            final String attr = String.format(
                "%s ↦ %s",
                ent.getKey(),
                ent.getValue().φTerm()
            );
            list.add(attr);
        }
        this.terms.get().remove(this.vertex);
        Collections.sort(list);
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
                "ν%d·%s⟦\n\t%s\n⟧", this.vertex, txt,
                new Indented(String.join(",\n", list))
            );
        }
        return txt;
    }

    @Override
    public String toString() {
        return this.toStringWith();
    }

    @Override
    public final Phi copy() {
        try {
            final PhDefault copy = PhDefault.class.cast(this.clone());
            copy.vertex = PhDefault.VTX.next();
            copy.cached = new CachedPhi();
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
    public final void move(final Phi rho) {
        this.attrs.put("ρ", new AtSimple(rho));
    }

    @Override
    public final Attr attr(final int pos) {
        if (pos < 0) {
            throw new ExFailure(
                String.format(
                    "Attribute position can't be negative (%d)",
                    pos
                )
            );
        }
        if (this.order.isEmpty()) {
            throw new ExFailure(
                String.format(
                    "There are no attributes here, can't read the %d-th one",
                    pos
                )
            );
        }
        int idx;
        for (idx = 0; idx < pos; ++idx) {
            if (idx >= this.order.size()) {
                throw new ExFailure(
                    String.format(
                        "There are just %d attributes here, can't read the %d-th one",
                        this.order.size(), pos
                    )
                );
            }
            final String name = this.order.get(idx);
            if (this.attrs.get(name) instanceof AtVararg) {
                break;
            }
        }
        return this.attr(this.order.get(idx));
    }

    @Override
    public final Attr attr(final String name) {
        Attr attr;
        if ("ν".equals(name)) {
            attr = new AtSimple(new Data.ToPhi((long) this.hashCode()));
        } else {
            attr = this.attrs.get(name);
        }
        if (attr == null) {
            final Attr aphi = this.attrs.get("φ");
            if (aphi == null) {
                attr = new AtAbsent(
                    name,
                    String.format(
                        " among other %d attrs (%s) and φ is absent",
                        this.attrs.size(),
                        String.join(", ", this.attrs.keySet())
                    )
                );
            } else {
                final Phi phi = this.cached.get(name, aphi::get);
                final Phi found = phi.attr(name).get();
                found.move(this);
                return new AtSimple(found);
            }
        }
        attr = this.named(attr, name);
        if ("φ".equals(name)) {
            attr = new AtPhiSensitive(attr, this.cached);
        }
        if (this.getClass().isAnnotationPresent(Volatile.class)) {
            this.cached.reset();
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
     * Make a string with this additional list of lines.
     * @param lines Lines to show in addition
     * @return The string
     */
    protected String toStringWith(final String... lines) {
        if (this.strings.get() == null) {
            this.strings.set(new HashSet<>());
        }
        if (this.strings.get().contains(this.vertex)) {
            return String.format("ν%d", this.vertex);
        }
        this.strings.get().add(this.vertex);
        final Collection<String> list = new ArrayList<>(this.attrs.size());
        for (final String line : lines) {
            list.add(new Indented(line).toString());
        }
        list.add(String.format("▸order=%s", new Indented(this.order)));
        list.add(String.format("▸cached=%s", new Indented(this.cached)));
        final List<String> sorted = new ArrayList<>(this.attrs.size());
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet()) {
            final int idx = this.order.indexOf(ent.getKey());
            sorted.add(
                String.format(
                    "%s%s=%s",
                    ent.getKey(),
                    idx >= 0 ? String.format("(#%d)", idx) : "",
                    new Indented(ent.getValue())
                )
            );
        }
        this.strings.get().remove(this.vertex);
        Collections.sort(sorted);
        list.addAll(sorted);
        return String.format(
            "%sν%d:{\n  %s\n}",
            this.getClass().getCanonicalName(),
            this.vertex,
            String.join("\n\t", list)
        );
    }

}
