/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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
     * The runtime root Java package that hosts all atoms, stripped from forma.
     */
    private static final Pattern ROOT = Pattern.compile("^org\\.eolang\\.?");

    /**
     * Declared return types of all atoms, loaded once from the generated table.
     */
    private static final AtomTypes ATOMS = PhDefault.atoms();

    /**
     * From Java package name to forma.
     */
    private static final Pattern TO_FORMA = Pattern.compile("(^|\\.)EO_?");

    /**
     * No initial attributes.
     */
    private static final Map<String, Attribute> NONE = Collections.emptyMap();

    /**
     * Structural attribute names that can never be a package extension, so a
     * miss on them must not probe the classpath.
     */
    private static final Set<String> SPECIAL = Set.of(Phi.LAMBDA, Phi.PHI, Phi.RHO);

    /**
     * Data.
     * @checkstyle VisibilityModifierCheck (2 lines)
     */
    private final byte[] data;

    /**
     * The forma of this object, taken from its XMIR locator; empty when the
     * object is anonymous and forma is derived from the Java class instead.
     */
    private final String fqn;

    /**
     * Initial attributes supplied via constructor; wrapped lazily.
     */
    private final Map<String, Attribute> initial;

    /**
     * Order of their names.
     */
    private Map<Integer, String> order;

    /**
     * Attributes.
     */
    private Map<String, Attribute> attrs;

    /**
     * Default ctor.
     */
    public PhDefault() {
        this("", null, PhDefault.NONE);
    }

    /**
     * Ctor with the forma taken from XMIR.
     * @param forma The forma of the object
     */
    public PhDefault(final String forma) {
        this(forma, null, PhDefault.NONE);
    }

    /**
     * Ctor with initial attributes.
     * @param attributes Initial attributes to register
     */
    public PhDefault(final Map<String, Attribute> attributes) {
        this("", null, attributes);
    }

    /**
     * Ctor.
     * @param dta Object data
     */
    public PhDefault(final byte[] dta) {
        this("", dta, PhDefault.NONE);
    }

    /**
     * Ctor.
     * @param dta        Object data
     * @param attributes Initial attributes to register
     */
    public PhDefault(final byte[] dta, final Map<String, Attribute> attributes) {
        this("", dta, attributes);
    }

    /**
     * Primary ctor.
     * @param forma      The forma of the object, taken from XMIR
     * @param dta        Object data
     * @param attributes Initial attributes to register
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private PhDefault(
        final String forma, final byte[] dta, final Map<String, Attribute> attributes
    ) {
        this.fqn = forma;
        this.data = dta;
        this.initial = attributes;
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
            this.activate();
            final PhDefault copy = (PhDefault) this.clone();
            final Map<String, Attribute> map = new HashMap<>(this.attrs.size());
            for (final Map.Entry<String, Attribute> ent : this.attrs.entrySet()) {
                map.put(ent.getKey(), ent.getValue().copy(copy));
            }
            copy.attrs = map;
            return copy;
        } catch (final CloneNotSupportedException ex) {
            throw new ExFailure("cannot copy the object", ex);
        }
    }

    @Override
    public boolean hasRho() {
        this.activate();
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
        this.put(this.vacancy(pos), object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.activate();
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
        this.activate();
        PhDefault.NESTING.set(PhDefault.NESTING.get() + 1);
        try {
            final Phi object;
            if (this.attrs.containsKey(name)) {
                object = this.attrs.get(name).get();
            } else if (name.equals(Phi.LAMBDA)) {
                object = new AtomTyped(
                    this, PhDefault.ATOMS.declared(this.forma())
                ).lambda();
            } else if (this instanceof Atom) {
                object = this.take(Phi.LAMBDA).take(name);
            } else if (this.attrs.containsKey(Phi.PHI)) {
                object = this.take(Phi.PHI).take(name);
            } else {
                object = new PhTerminator();
            }
            final Phi resolved;
            if (object instanceof PhTerminator) {
                resolved = this.extension(name, object);
            } else {
                resolved = object;
            }
            PhDefault.debug(
                String.format(
                    "%s\uD835\uDD38('%s' for %s) ➜ %s",
                    PhDefault.padding(),
                    name,
                    this,
                    resolved
                )
            );
            return resolved;
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
        this.activate();
        final byte[] bytes;
        if (this.data != null) {
            bytes = this.data;
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
    public Phi normalized() {
        this.activate();
        final Phi result;
        if (this instanceof Atom) {
            result = this.take(Phi.LAMBDA).normalized();
        } else if (this.data == null && this.attrs.containsKey(Phi.PHI)) {
            final Phi phi = this.take(Phi.PHI).normalized();
            if (phi instanceof PhTerminator) {
                result = phi;
            } else {
                result = this;
            }
        } else {
            result = this;
        }
        return result;
    }

    @Override
    public String locator() {
        return "?";
    }

    @Override
    public String forma() {
        final String form;
        if (this.fqn.isEmpty()) {
            form = this.derived();
        } else {
            form = this.fqn;
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
    public final void add(final String name, final Attribute attr) {
        this.activate();
        if (PhDefault.SORTABLE.matcher(name).matches()) {
            this.order.put(this.order.size(), name);
        }
        this.attrs.put(name, new AtWithRho(attr, this));
    }

    @Override
    public String φTerm() {
        this.activate();
        final String name = this.oname();
        final String result;
        if (this.literal(name)) {
            final byte[] raw = this.attrs.get("as-bytes").get().delta();
            if ("string".equals(name)) {
                result = String.format("\"%s\"", new String(raw, StandardCharsets.UTF_8));
            } else {
                result = PhDefault.numeral(new BytesOf(raw).asNumber());
            }
        } else {
            result = this.structural();
        }
        return result;
    }

    /**
     * Render a number value as a φ-term.
     * @param value The number
     * @return Whole values without a fraction, decimals otherwise
     */
    static String numeral(final double value) {
        final String txt;
        if (value == Math.floor(value) && !Double.isInfinite(value)) {
            txt = Long.toString((long) value);
        } else {
            txt = Double.toString(value);
        }
        return txt;
    }

    /**
     * Derive the forma from the Java class, when the object has no XMIR locator.
     * @return The forma built from the class name and its package
     */
    private String derived() {
        final String form;
        final String name = this.oname();
        if (PhDefault.class.getSimpleName().equals(name)) {
            form = "[]";
        } else {
            form = this.packaged(name);
        }
        return form;
    }

    /**
     * Resolve an absent attribute as an object living in this object's own
     * package.
     *
     * <p>When {@code (number 42).power} finds no {@code power} attribute, the
     * runtime looks for an object {@code Φ.number.power} in the package named
     * after this object's forma. If it exists, it is returned with this object
     * bound as its first argument ({@code α0}), so {@code (number 42).power 3}
     * reads as {@code number.power 42 3}. The receiver of a package extension
     * always lives in {@code α0}: implicit dispatch binds it here, while
     * explicit dispatch through the namespace ({@code number.power 42 3})
     * leaves the slot for the caller to fill (see {@code PhNest.extension}).
     * When there's no such object, the terminated
     * computation stays terminated. When the object exists but has no free
     * positional attribute to receive the bound object (for example a nullary
     * object like {@code os}), a clear error is raised instead of the
     * low-level "attribute is already set / no attributes here" message.</p>
     *
     * @param name The name of the absent attribute
     * @param bottom The terminated computation to keep when there's no such object
     * @return The package object with this bound, or the given {@code bottom}
     */
    private Phi extension(final String name, final Phi bottom) {
        final String forma = this.forma();
        Phi found = bottom;
        if (!PhDefault.SPECIAL.contains(name)
            && forma.startsWith(String.format("%s.", PhPackage.GLOBAL))) {
            final String full = String.join(".", forma, name);
            if (OnClasspath.has(new JavaPath(full).toString())) {
                final Phi taken = Phi.Φ.take(full.substring(PhPackage.GLOBAL.length() + 1));
                try {
                    taken.put(0, this);
                } catch (final ExAbstract ex) {
                    throw new ExFailure(
                        String.format(
                            "Object '%s' takes no arguments, so it can't be applied to '%s' via the implicit '%s' form",
                            full, forma, name
                        ),
                        ex
                    );
                }
                found = taken;
            }
        }
        return found;
    }

    /**
     * Build the forma from the package of this object and the given name.
     * @param name The object name, as in source code
     * @return The fully-qualified forma
     */
    private String packaged(final String name) {
        final String form;
        final String pkg = PhDefault.TO_FORMA.matcher(
            PhDefault.ROOT.matcher(this.getClass().getPackageName()).replaceAll("")
        ).replaceAll("$1");
        if (pkg.isEmpty()) {
            form = String.join(".", PhPackage.GLOBAL, name);
        } else {
            form = String.join(".", PhPackage.GLOBAL, pkg, name);
        }
        return form;
    }

    /**
     * Resolve the attribute name for a positional write, continuing partial
     * application when needed.
     *
     * <p>The attribute at the requested position is used as-is when it can
     * still receive a value. When it's a void that has already been set — as
     * happens when a curried object is fed its next argument — the write is
     * redirected to the first still-unset void that follows, in declaration
     * order. This is the only case whose behavior changes: previously such a
     * write threw {@link ExReadOnly}. Positions that point at non-void
     * attributes keep failing, so passing too many arguments is still
     * reported.</p>
     *
     * @param pos Position of the attribute
     * @return The name of the attribute to write to
     */
    private String vacancy(final int pos) {
        String name = this.attr(pos);
        if (!this.attrs.get(name).vacant()) {
            for (int idx = pos + 1; idx < this.order.size(); ++idx) {
                final String next = this.order.get(idx);
                if (this.attrs.get(next).vacant()) {
                    name = next;
                    break;
                }
            }
        }
        return name;
    }

    /**
     * Get attribute name by position.
     * @param pos Position of the attribute
     * @return Attribute name
     */
    private String attr(final int pos) {
        this.activate();
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
     * Activate the lazy state: initialize attrs/order from the constructor-supplied
     * map, wrapping each entry with {@link AtWithRho}. Idempotent.
     */
    private void activate() {
        if (this.attrs == null) {
            this.attrs = PhDefault.defaults();
            this.order = new HashMap<>(0);
            for (final Map.Entry<String, Attribute> ent : this.initial.entrySet()) {
                this.add(ent.getKey(), ent.getValue());
            }
        }
    }

    /**
     * Is this a number or string object with injected bytes inside.
     * @param name Object name, as in source code
     * @return True if its value can be rendered directly
     */
    private boolean literal(final String name) {
        return ("number".equals(name) || "string".equals(name))
            && this.attrs.containsKey("as-bytes")
            && !"?".equals(this.attrs.get("as-bytes").φTerm());
    }

    /**
     * Structural φ-term, listing the attributes of this object.
     * @return The φ-term
     */
    private String structural() {
        final List<String> list = new ArrayList<>(this.attrs.size());
        if (this.data != null) {
            list.add(String.format("D> %s", PhDefault.termBytes(this.data)));
        }
        for (final Map.Entry<String, Attribute> ent : this.attrs.entrySet().stream().filter(
            e -> !e.getKey().equals(Phi.RHO)
        ).collect(Collectors.toList())) {
            list.add(String.format("%s->%s", ent.getKey(), ent.getValue().φTerm()));
        }
        if (this instanceof Atom) {
            list.add(String.format("L> %s", this.getClass().getSimpleName()));
        }
        Collections.sort(list);
        final String term;
        if (list.isEmpty()) {
            term = "[]";
        } else {
            term = String.format("[%s]", String.join(",", list));
        }
        return term;
    }

    /**
     * Bytes representation for the φ-term D> slot.
     * This is intentionally not {@link Bytes#asString()}.
     * @param data Bytes
     * @return Bytes as shown in φ-terms
     */
    private static String termBytes(final byte[] data) {
        final String result;
        if (data.length == 0) {
            result = "--";
        } else {
            final StringBuilder out = new StringBuilder(data.length * 3);
            for (final byte bte : data) {
                if (out.length() > 0) {
                    out.append('-');
                }
                out.append(String.format("%02X", bte));
            }
            result = out.toString();
        }
        return result;
    }

    /**
     * Default attributes hash map with RHO attribute put.
     * @return Default attributes hash map
     */
    private static Map<String, Attribute> defaults() {
        final Map<String, Attribute> attrs = new HashMap<>(0);
        attrs.put(Phi.RHO, new AtRho());
        return attrs;
    }

    /**
     * Load the declared return types of all atoms from the generated table.
     * @return The atom types table, empty when the table is absent
     */
    private static AtomTypes atoms() {
        final Map<String, String> table;
        final InputStream source = PhDefault.class.getResourceAsStream("atoms.csv");
        if (source == null) {
            table = Collections.emptyMap();
        } else {
            try (
                BufferedReader lines = new BufferedReader(
                    new InputStreamReader(source, StandardCharsets.UTF_8)
                )
            ) {
                table = lines.lines().filter(line -> line.contains(",")).collect(
                    Collectors.toMap(
                        line -> line.substring(0, line.indexOf(',')),
                        line -> line.substring(line.indexOf(',') + 1)
                    )
                );
            } catch (final IOException ex) {
                throw new ExFailure("Failed to read the atom types table", ex);
            }
        }
        return new AtomTypes(table);
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
     * @return Padding string
     */
    private static String padding() {
        return String.join("", Collections.nCopies(PhDefault.NESTING.get(), "·"));
    }
}
