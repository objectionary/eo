/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

/**
 * A simple object.
 *
 * <p>The class is thread-safe.</p>
 *
 * @since 0.1
 * @checkstyle DesignForExtensionCheck (500 lines)
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
public class PhDefault implements Phi, Cloneable {
    /**
     * Logger.
     */
    private static final Logger LOGGER = Logger.getLogger(PhDefault.class.getName());

    /**
     * Attribute name matcher.
     */
    private static final Pattern SORTABLE = Pattern.compile(
        String.format("^([a-z].*)|%s$", Phi.PHI)
    );

    /**
     * Attributes nesting level.
     */
    @SuppressWarnings("java:S5164")
    private static final ThreadLocal<Integer> NESTING = ThreadLocal.withInitial(() -> 0);

    /**
     * From Java package name to forma.
     */
    private static final Pattern TO_FORMA = Pattern.compile("(^|\\.)EO");

    /**
     * Data.
     * @checkstyle VisibilityModifierCheck (2 lines)
     */
    private final Optional<byte[]> data;

    /**
     * Order of their names.
     */
    private final Map<Integer, String> order;

    /**
     * Attributes.
     */
    private Map<String, Attr> attrs;

    /**
     * Default ctor.
     */
    public PhDefault() {
        this(null);
    }

    /**
     * Ctor.
     * @param dta Object data
     */
    public PhDefault(final byte[] dta) {
        this.data = Optional.ofNullable(dta);
        this.attrs = PhDefault.defaults();
        this.order = new HashMap<>(0);
    }

    @Override
    public boolean equals(final Object obj) {
        return obj instanceof Phi && this.hashCode() == obj.hashCode();
    }

    @Override
    public int hashCode() {
        return super.hashCode() + 1;
    }

    @Override
    public final Phi copy() {
        try {
            final PhDefault copy = (PhDefault) this.clone();
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
    public boolean hasRho() {
        boolean has = true;
        try {
            this.attrs.get(Phi.RHO).get();
        } catch (final ExUnset exception) {
            has = false;
        }
        return has;
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.put(this.attr(pos), object);
    }

    @Override
    public void put(final String name, final Phi object) {
        if (!this.attrs.containsKey(name)) {
            throw new ExUnset(
                String.format(
                    "Can't #put(\"%s\", %s) to %s, because the attribute is absent",
                    name, object, this
                )
            );
        }
        this.attrs.get(name).put(object);
    }

    @Override
    public Phi take(final String name) {
        PhDefault.NESTING.set(PhDefault.NESTING.get() + 1);
        try {
            final Phi object;
            if (this.attrs.containsKey(name)) {
                object = this.attrs.get(name).get();
            } else if (name.equals(Phi.LAMBDA)) {
                object = new AtomSafe(this).lambda();
            } else if (this instanceof Atom) {
                object = this.take(Phi.LAMBDA).take(name);
            } else if (this.attrs.containsKey(Phi.PHI)) {
                object = this.take(Phi.PHI).take(name);
            } else {
                throw new ExUnset(
                    String.format(
                        "Can't #take(\"%s\"), the attribute is absent among other %d attrs of %s:(%s), %s and %s are also absent",
                        name,
                        this.attrs.size(),
                        this.forma(),
                        String.join(", ", this.attrs.keySet()),
                        Phi.PHI,
                        Phi.LAMBDA
                    )
                );
            }
            PhDefault.debug(
                String.format(
                    "%s\uD835\uDD38('%s' for %s) ➜ %s",
                    PhDefault.padding(),
                    name,
                    this,
                    object
                )
            );
            return object;
        } finally {
            final int current = PhDefault.NESTING.get();
            if (current > 0) {
                PhDefault.NESTING.set(current - 1);
            } else {
                PhDefault.NESTING.set(0);
            }
        }
    }

    @Override
    public byte[] delta() {
        final byte[] bytes;
        if (this.data.isPresent()) {
            bytes = this.data.get();
        } else if (this instanceof Atom) {
            bytes = this.take(Phi.LAMBDA).delta();
        } else if (this.attrs.containsKey(Phi.PHI)) {
            bytes = this.take(Phi.PHI).delta();
        } else {
            throw new ExFailure(
                String.format(
                    "There's no \"Δ\" in the object of \"%s\"",
                    this.forma()
                )
            );
        }
        return bytes;
    }

    @Override
    public String locator() {
        return "?";
    }

    @Override
    public String forma() {
        final String name = this.oname();
        final String form;
        if (PhDefault.class.getSimpleName().equals(name)) {
            form = "[]";
        } else {
            form = String.join(
                ".",
                PhPackage.GLOBAL,
                PhDefault.TO_FORMA.matcher(
                    this.getClass().getPackageName()
                ).replaceAll("$1"),
                name
            );
        }
        return form;
    }

    /**
     * Add new attribute.
     *
     * <p>This method can only be called from child classes, in their
     * constructors, when they declare their attributes. This is why it's
     * protected.</p>
     *
     * @param name The name
     * @param attr The attr
     */
    public final void add(final String name, final Attr attr) {
        if (PhDefault.SORTABLE.matcher(name).matches()) {
            this.order.put(this.order.size(), name);
        }
        this.attrs.put(name, new AtWithRho(attr, this));
    }

    /**
     * Get attribute name by position.
     * @param pos Position of the attribute
     * @return Attribute name
     */
    private String attr(final int pos) {
        if (0 > pos) {
            throw new ExFailure(
                String.format(
                    "The attribute position can't be negative (%d)",
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
        if (!this.order.containsKey(pos)) {
            throw new ExFailure(
                String.format(
                    "%s has just %d attribute(s), can't read the %d-th one",
                    this,
                    this.order.size(),
                    pos
                )
            );
        }
        return this.order.get(pos);
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
     * Default attributes hash map with RHO attribute put.
     * @return Default attributes hash map
     */
    private static Map<String, Attr> defaults() {
        final Map<String, Attr> attrs = new HashMap<>(0);
        attrs.put(Phi.RHO, new AtRho());
        return attrs;
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
