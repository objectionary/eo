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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * A simple object.
 *
 * <p>The class is thread-safe.</p>
 *
 * @since 0.1
 * @checkstyle DesignForExtensionCheck (500 lines)
 */
@Versionized
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
public class PhDefault implements Phi, Cloneable {
    /**
     * Logger.
     */
    private static final Logger LOGGER = Logger.getLogger(PhDefault.class.getName());

    /**
     * Attribute name matcher.
     */
    private static final Pattern SORTABLE = Pattern.compile("^[a-z].*$");

    /**
     * Attributes nesting level.
     *
     * @todo #2251:90min It is necessary to call {@link ThreadLocal#remove()} on
     *  {@link PhDefault#NESTING} to prevent memory leaks. We should either find a place where this
     *  variable can be removed, or, if this is not possible
     *  (see https://github.com/objectionary/eo/pull/1930), come up with another solution.
     */
    @SuppressWarnings("java:S5164")
    private static final ThreadLocal<Integer> NESTING = ThreadLocal.withInitial(() -> 0);

    /**
     * From Java class name to forma.
     */
    private static final Pattern TO_FORMA = Pattern.compile("(^|\\.)EO");

    /**
     * Data.
     * @checkstyle VisibilityModifierCheck (2 lines)
     */
    private final Optional<byte[]> data;

    /**
     * Forma of it.
     */
    private final String form;

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
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public PhDefault(final byte[] dta) {
        this.data = Optional.ofNullable(dta);
        this.form = PhDefault.TO_FORMA.matcher(this.getClass().getName()).replaceAll("$1");
        this.attrs = new HashMap<>(0);
        this.order = new HashMap<>(0);
        this.add(Attr.RHO, new AtRho());
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
    public String φTerm() {
        final List<String> list = new ArrayList<>(this.attrs.size());
        final String format = "%s ↦ %s";
        this.data.ifPresent(
            bytes -> list.add(
                String.format(
                    format, Attr.DELTA, new BytesOf(bytes).asString()
                )
            )
        );
        for (final Map.Entry<String, Attr> ent : this.attrs.entrySet().stream().filter(
            e -> !e.getKey().equals(Attr.RHO)
        ).collect(Collectors.toList())) {
            final String attr = String.format(
                format,
                ent.getKey(),
                ent.getValue().φTerm()
            );
            list.add(attr);
        }
        if (this instanceof Atom) {
            list.add(String.format(format, Attr.LAMBDA, "Lambda"));
        }
        Collections.sort(list);
        String txt = this.oname();
        if (!list.isEmpty()) {
            txt = String.format(
                "ν%d·%s⟦\n\t%s\n⟧", this.hashCode(), txt,
                new Indented(String.join(",\n", list))
            );
        }
        return txt;
    }

    @Override
    public String toString() {
        String result = String.format(
            "%sν%d",
            this.getClass().getCanonicalName(),
            this.hashCode()
        );
        if (this.data.isPresent()) {
            result = String.format(
                "%s=%s",
                result,
                new BytesOf(this.data.get()).asString()
            );
        }
        return result;
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
    public boolean put(final int pos, final Phi object) {
        return this.put(this.attr(pos), object);
    }

    @Override
    public boolean put(final String name, final Phi object) {
        if (!this.attrs.containsKey(name)) {
            throw new ExUnset(
                String.format(
                    "Can't #put(\"%s\", %s) to %s, because the attribute is absent",
                    name, object, this
                )
            );
        }
        return this.attrs.get(name).put(object);
    }

    @Override
    public Phi take(final String name) {
        PhDefault.NESTING.set(PhDefault.NESTING.get() + 1);
        final Phi object;
        if (this.attrs.containsKey(name)) {
            object = new AtSetRho(
                this.attrs.get(name),
                this,
                name
            ).get();
        } else if (name.equals(Attr.LAMBDA)) {
            object = new AtSetRho(
                new AtFormed(new AtomSafe((Atom) this)::lambda),
                this,
                name
            ).get();
        } else if (this instanceof Atom) {
            object = this.take(Attr.LAMBDA).take(name);
        } else if (this.attrs.containsKey(Attr.PHI)) {
            object = this.take(Attr.PHI).take(name);
        } else {
            object = new AtGetOnly(
                () -> {
                    throw new ExUnset(
                        String.format(
                            "Can't #take(\"%s\"), the attribute is absent among other %d attrs of %s:(%s), %s and %s are also absent",
                            name,
                            this.attrs.size(),
                            this.form,
                            String.join(", ", this.attrs.keySet()),
                            Attr.PHI,
                            Attr.LAMBDA
                        )
                    );
                }
            ).get();
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
        PhDefault.NESTING.set(PhDefault.NESTING.get() - 1);
        return object;
    }

    @Override
    public byte[] delta() {
        final byte[] bytes;
        if (this.data.isPresent()) {
            bytes = this.data.get();
        } else if (this instanceof Atom) {
            bytes = this.take(Attr.LAMBDA).delta();
        } else if (this.attrs.containsKey(Attr.PHI)) {
            bytes = this.take(Attr.PHI).delta();
        } else {
            throw new ExFailure(
                String.format(
                    "There's no data in the object of %s",
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
        return this.form;
    }

    /**
     * Add new attribute.
     *
     * <p>This method can only be called from child classes, in their
     * constructors, when they declare their attributes. This is why it's
     * protected. Not the brightest design, I admit.</p>
     *
     * @param name The name
     * @param attr The attr
     */
    protected final void add(final String name, final Attr attr) {
        if (PhDefault.SORTABLE.matcher(name).matches()) {
            this.order.put(this.order.size(), name);
        }
        this.attrs.put(name, attr);
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
