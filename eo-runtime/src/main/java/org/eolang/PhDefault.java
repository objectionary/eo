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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * A simple object.
 *
 * The class is thread-safe.
 * @checkstyle DesignForExtensionCheck (500 lines)
 * @since 0.1
 */
@Versionized
@SuppressWarnings({"PMD.TooManyMethods", "PMD.ConstructorShouldDoInitialization"})
public abstract class PhDefault implements Phi, Cloneable {

    /**
     * Vertices.
     */
    protected static final Vertices VTX = new Vertices();

    /**
     * Logger.
     */
    private static final Logger LOGGER = Logger.getLogger(PhDefault.class.getName());

    /**
     * Attribute name matcher.
     */
    private static final Pattern SORTABLE = Pattern.compile("^[a-z].*$");

    /**
     * Terms being processed now.
     */
    private static final ThreadLocal<Set<Integer>> TERMS = new ThreadLocal<>();

    /**
     * Attributes nesting level.
     */
    private static final ThreadLocal<Integer> NESTING = ThreadLocal.withInitial(() -> 0);

    /**
     * Identity of it (the ID of the vertex).
     * @checkstyle VisibilityModifierCheck (2 lines)
     */
    protected int vertex;

    /**
     * Forma of it.
     */
    private String form;

    /**
     * Order of their names.
     */
    private final List<String> order;

    /**
     * Attributes.
     */
    private Map<String, Attr> attrs;

    /**
     * Cached \phi.
     */
    private CachedPhi cached = new CachedPhi();

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
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public PhDefault(final Phi sigma) {
        this.vertex = PhDefault.VTX.next();
        this.form = this.getClass().getName();
        this.attrs = new HashMap<>(0);
        this.order = new ArrayList<>(0);
        this.add("ρ", new AtSimple(sigma));
        this.add("σ", new AtFixed(new AtSimple(sigma)));
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
        if (null == PhDefault.TERMS.get()) {
            PhDefault.TERMS.set(new HashSet<>());
        }
        String txt;
        if (PhDefault.TERMS.get().contains(this.vertex)) {
            txt = String.format("ν%d", this.vertex);
        } else {
            PhDefault.TERMS.get().add(this.vertex);
            final List<String> list = new ArrayList<>(this.attrs.size());
            for (final Map.Entry<String, Attr> ent : this.attrs.entrySet().stream().filter(
                e -> Arrays.asList("σ", "ρ", "Δ").contains(e.getKey())
            ).collect(Collectors.toList())) {
                final String attr = String.format(
                    "%s ↦ %s",
                    ent.getKey(),
                    ent.getValue().φTerm()
                );
                list.add(attr);
            }
            PhDefault.TERMS.get().remove(this.vertex);
            Collections.sort(list);
            txt = this.oname();
            if (!list.isEmpty()) {
                txt = String.format(
                    "ν%d·%s⟦\n\t%s\n⟧", this.vertex, txt,
                    new Indented(String.join(",\n", list))
                );
            }
        }
        return txt;
    }

    @Override
    public String toString() {
        String result = String.format(
            "%sν%d",
            this.getClass().getCanonicalName(),
            this.vertex
        );
        if (this.attrs.containsKey("Δ")) {
            result = String.format(
                "%s=%s",
                result,
                this.attrs.get("Δ").toString()
            );
        }
        return result;
    }

    @Override
    public final Phi copy() {
        try {
            final PhDefault copy = (PhDefault) this.clone();
            copy.vertex = PhDefault.VTX.next();
            copy.form = this.form;
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
    public final Attr attr(final int pos) {
        if (0 > pos) {
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
        Attr attr = this.attr(this.order.get(0));
        for (int idx = 0; idx <= pos; ++idx) {
            if (idx >= this.order.size()) {
                throw new ExFailure(
                    String.format(
                        "%s has just %d attribute(s), can't read the %d-th one",
                        this,
                        this.order.size(),
                        pos
                    )
                );
            }
            attr = this.attr(this.order.get(idx));
        }
        return attr;
    }

    @Override
    public final Attr attr(final String name) {
        PhDefault.NESTING.set(PhDefault.NESTING.get() + 1);
        Attr attr;
        if ("ν".equals(name)) {
            attr = new AtSimple(new Data.ToPhi((long) this.hashCode()));
        } else {
            attr = this.attrs.get(name);
        }
        if (attr == null) {
            final Phi found;
            if (this instanceof Atom) {
                found = this.cached.get(name, new AtomSafe((Atom) this)::lambda).attr(name).get();
                found.attr("ρ").put(this);
                attr = new AtSimple(found);
            } else if (this.attrs.containsKey(Attr.PHI)) {
                final Attr aphi = this.attrs.get(Attr.PHI);
                found = this.cached.get(name, aphi::get).attr(name).get();
                found.attr("ρ").put(this);
                attr = new AtSimple(found);
            } else {
                attr = new AtAbsent(
                    name,
                    String.format(
                        " among other %d attrs (%s) and %s is absent",
                        this.attrs.size(),
                        String.join(", ", this.attrs.keySet()),
                        Attr.PHI
                    )
                );
            }
        }
        attr = this.named(attr, name);
        if (Attr.PHI.equals(name)) {
            attr = new AtPhiSensitive(attr, this.cached);
        }
        if (this.getClass().isAnnotationPresent(Volatile.class)) {
            this.cached.reset();
        }
        attr = new AtSafe(attr);
        PhDefault.debug(
            String.format(
                "%s\uD835\uDD38('%s' for %s) ➜ %s",
                PhDefault.padding(),
                name,
                this,
                attr
            )
        );
        PhDefault.NESTING.set(PhDefault.NESTING.get() - 1);
        return attr;
    }

    @Override
    public String locator() {
        return "?";
    }

    @Override
    public String forma() {
        return this.form;
    }

    /**
     * Clean up resources.
     * This includes call of {@link ThreadLocal#remove()} method to prevent
     * memory leaks.
     */
    public static void cleanUp() {
        PhDefault.TERMS.remove();
        PhDefault.NESTING.remove();
    }

    /**
     * Add new attribute.
     *
     * This method can only be called from child classes, in their
     * constructors, when they declare their attributes. This is why it's
     * protected. Not the brightest design, I admit.
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
        if (null != xmir) {
            txt = xmir.oname();
            if ("@".equals(txt)) {
                txt = "φ";
            }
        }
        return txt;
    }

    /**
     * Log debug message for PhDefault.
     * @param msg Message to log
     */
    private static void debug(final String msg) {
        if (PhDefault.LOGGER.isLoggable(Level.FINE)) {
            PhDefault.LOGGER.log(
                Level.FINE,
                msg
            );
        }
    }

    /**
     * Padding according to current {@link #NESTING} level.
     * @return Padding string.
     */
    private static String padding() {
        return String.join("", Collections.nCopies(PhDefault.NESTING.get(), "·"));
    }
}
