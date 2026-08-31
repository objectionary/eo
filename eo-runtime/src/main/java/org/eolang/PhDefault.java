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
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
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
@SuppressWarnings("PMD.GodClass")
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
     * Data.
     */
    private final Snapshot data;

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
     *
     * <p>Not final: {@link #copy()} gives the copy a list of its own, so an
     * attribute registered on either side afterwards is not seen by the
     * other one.</p>
     */
    private List<String> order;

    /**
     * Attributes.
     */
    private Map<String, Attribute> attrs;

    /**
     * Guards {@link #attrs} and {@link #order} against concurrent lazy init.
     * Not final: {@link #copy()} gives the copy its own, since a copy's
     * lazy init is independent of the origin's.
     */
    private ReentrantLock lock;

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
     */
    private PhDefault(
        final String forma, final byte[] dta, final Map<String, Attribute> attributes
    ) {
        this.fqn = forma;
        this.data = new Snapshot(dta);
        this.initial = attributes;
        this.order = new ArrayList<>(0);
        this.lock = new ReentrantLock();
    }

    @Override
    public boolean equals(final Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this) + 1;
    }

    @Override
    public final Phi copy() {
        try {
            final PhDefault copy = (PhDefault) this.clone();
            copy.lock = new ReentrantLock();
            final CopiedAttrs fresh = new CopiedAttrs(this.loaded(), copy);
            fresh.freeze();
            copy.attrs = fresh;
            copy.order = new ArrayList<>(this.order);
            return copy;
        } catch (final CloneNotSupportedException ex) {
            throw new ExFailure("cannot copy the object", ex);
        }
    }

    @Override
    public boolean needsRho() {
        final Attribute attr = this.loaded().get(Phi.RHO);
        return attr != null && attr.vacant();
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.lock.lock();
        try {
            this.put(this.vacancy(pos), object);
        } finally {
            this.lock.unlock();
        }
    }

    @Override
    public void put(final String name, final Phi object) {
        final Attribute attr = this.loaded().get(name);
        if (attr == null) {
            throw new ExUnset(
                String.format(
                    "Can't #put(\"%s\", %s) to %s, because the attribute is absent",
                    name, object, this
                )
            );
        }
        attr.put(object);
    }

    @Override
    public Phi take(final String name) {
        if (Thread.currentThread().isInterrupted()) {
            throw new ExInterrupted(
                String.format(
                    "Can't take \"%s\" from %s, because the thread was interrupted",
                    name, this.forma()
                )
            );
        }
        final boolean logging = PhDefault.LOGGER.isLoggable(Level.FINE);
        if (logging) {
            PhDefault.NESTING.set(PhDefault.NESTING.get() + 1);
        }
        try {
            final Phi resolved;
            final Attribute attr = this.loaded().get(name);
            if (attr != null) {
                resolved = attr.get();
            } else if (name.equals(Phi.LAMBDA) && this instanceof Atom) {
                resolved = new AtomTyped(
                    this, PhDefault.ATOMS.declared(this.forma())
                ).lambda();
            } else {
                resolved = this.absent(name);
            }
            if (logging) {
                PhDefault.LOGGER.log(
                    Level.FINE,
                    String.format(
                        "%s𝔸('%s' for %s) ➜ %s",
                        PhDefault.padding(),
                        name,
                        this,
                        resolved
                    )
                );
            }
            return resolved;
        } finally {
            if (logging) {
                PhDefault.NESTING.set(Math.max(0, PhDefault.NESTING.get() - 1));
            }
        }
    }

    @Override
    public byte[] delta() {
        final byte[] bytes;
        if (this.data.present()) {
            bytes = this.data.bytes();
        } else if (this instanceof Atom) {
            bytes = this.take(Phi.LAMBDA).delta();
        } else if (this.loaded().containsKey(Phi.PHI)) {
            bytes = this.take(Phi.PHI).delta();
        } else {
            throw new ExFailure(
                "There's no \"Δ\" in the object of \"%s\"",
                this.forma()
            );
        }
        return bytes;
    }

    @Override
    public Phi normalized() {
        final Phi result;
        if (this instanceof Atom) {
            result = this.take(Phi.LAMBDA).normalized();
        } else if (this.data.empty() && this.loaded().containsKey(Phi.PHI)) {
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
     * <p>Public and overridable, so a subclass substituted for this one
     * (via the {@code eo.phiDefaultClass} compiler option) can observe
     * every attribute addition. {@link #loaded()} calls this method for
     * each attribute passed to the constructor, lazily on first access
     * rather than during construction, so an override sees the initial
     * attribute set materialize here too, not only later additions.</p>
     *
     * @param name The name
     * @param attr The attr
     */
    public void add(final String name, final Attribute attr) {
        this.lock.lock();
        try {
            if (PhDefault.SORTABLE.matcher(name).matches() && !this.order.contains(name)) {
                this.order.add(name);
            }
            if (Phi.RHO.equals(name)) {
                this.loaded().put(name, attr);
            } else {
                this.loaded().put(name, new AtWithRho(attr, this));
            }
        } finally {
            this.lock.unlock();
        }
    }

    @Override
    public String φTerm() {
        final String name = this.oname();
        final String result;
        if (this.literal(name)) {
            final byte[] raw = this.loaded().get("as-bytes").get().delta();
            if ("string".equals(name)) {
                result = new Quoted(raw).get().orElseGet(this::structural);
            } else {
                result = new Numeral(new BytesOf(raw).asNumber()).get();
            }
        } else {
            result = this.structural();
        }
        return result;
    }

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

    private Phi absent(final String name) {
        final Phi object;
        if (Phi.RHO.equals(name)) {
            object = new PhTerminator(
                String.format("the object %s declares no receiver", this.forma())
            );
        } else if (this instanceof Atom) {
            object = this.take(Phi.LAMBDA).take(name);
        } else if (this.loaded().containsKey(Phi.PHI)) {
            object = this.take(Phi.PHI).take(name);
        } else {
            object = new PhTerminator(
                String.format("the attribute \"%s\" is not found in %s", name, this.forma())
            );
        }
        return object;
    }

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

    private String vacancy(final int pos) {
        String name = this.attr(pos);
        if (!this.loaded().get(name).vacant()) {
            for (int idx = pos + 1; idx < this.order.size(); ++idx) {
                final String next = this.order.get(idx);
                if (this.loaded().get(next).vacant()) {
                    name = next;
                    break;
                }
            }
        }
        return name;
    }

    private String attr(final int pos) {
        this.loaded();
        if (0 > pos) {
            throw new ExFailure(
                "The attribute position can't be negative (%d)",
                pos
            );
        }
        if (this.order.isEmpty()) {
            throw new ExFailure(
                "There are no attributes here, can't read the %d-th one",
                pos
            );
        }
        if (pos >= this.order.size()) {
            throw new ExFailure(
                "%s has just %d attribute(s), can't read the %d-th one",
                this,
                this.order.size(),
                pos
            );
        }
        return this.order.get(pos);
    }

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

    private Map<String, Attribute> loaded() {
        this.lock.lock();
        try {
            if (this.attrs == null) {
                this.attrs = new Bindings();
                for (final Map.Entry<String, Attribute> ent : this.initial.entrySet()) {
                    this.add(ent.getKey(), ent.getValue());
                }
            }
            return this.attrs;
        } finally {
            this.lock.unlock();
        }
    }

    private boolean literal(final String name) {
        return ("number".equals(name) || "string".equals(name))
            && this.loaded().containsKey("as-bytes")
            && !"?".equals(this.loaded().get("as-bytes").φTerm());
    }

    private String structural() {
        final List<String> list = new ArrayList<>(this.loaded().size());
        if (this.data.present()) {
            list.add(String.format("D> %s", PhDefault.termBytes(this.data.bytes())));
        }
        for (final Map.Entry<String, Attribute> ent : this.loaded().entrySet().stream().filter(
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

    private static String termBytes(final byte[] data) {
        final String result;
        if (data.length == 0) {
            result = "--";
        } else if (data.length == 1) {
            result = String.format("%02X-", data[0]);
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

    private static String padding() {
        return String.join("", Collections.nCopies(PhDefault.NESTING.get(), "·"));
    }
}
