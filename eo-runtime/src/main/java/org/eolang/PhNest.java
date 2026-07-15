/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * An object that is also a package.
 *
 * <p>Some objects, like {@code number} or {@code string}, own a package of the
 * same name that holds extension objects (for example {@code number.power}).
 * When such a name is resolved from a parent package this object stands for
 * both of them at once: a namespace, whose {@link #take(String)} hands out the
 * extension objects as they are, and the object itself, to which it delegates
 * everything else and into which it collapses on {@link #copy()}. Thus
 * {@code number.power} reads the extension untouched, while {@code (number 42)}
 * copies into a plain number value.</p>
 *
 * @since 0.62
 */
final class PhNest implements Phi {

    /**
     * The forma of the package, and of the object, like {@code Φ.number}.
     */
    private final String pkg;

    /**
     * Extension objects already loaded from the package.
     */
    private final Map<String, Phi> objects;

    /**
     * The object itself, loaded lazily and cached under {@link #pkg}.
     */
    private final Map<String, Phi> origin;

    /**
     * Ctor.
     * @param name The forma of the package
     */
    PhNest(final String name) {
        this.pkg = name;
        this.objects = new ConcurrentHashMap<>(0);
        this.origin = new ConcurrentHashMap<>(1);
    }

    @Override
    public Phi take(final String name) {
        final Phi taken;
        if (!name.equals(Phi.RHO) && this.owns(name)) {
            taken = this.extension(name);
        } else {
            taken = this.object().take(name);
        }
        return taken;
    }

    @Override
    public Phi copy() {
        return this.object().copy();
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.object().put(pos, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.object().put(name, object);
    }

    @Override
    public byte[] delta() {
        return this.object().delta();
    }

    @Override
    public boolean hasRho() {
        return this.object().hasRho();
    }

    @Override
    public Phi normalized() {
        return this.object().normalized();
    }

    @Override
    public String forma() {
        return this.pkg;
    }

    @Override
    public String locator() {
        return this.object().locator();
    }

    @Override
    public String φTerm() {
        return this.object().φTerm();
    }

    /**
     * Does the package own an extension object with this name?
     * @param name The name of the extension
     * @return TRUE if it does
     */
    private boolean owns(final String name) {
        boolean owns;
        try {
            Class.forName(new JavaPath(String.join(".", this.pkg, name)).toString());
            owns = true;
        } catch (final ClassNotFoundException ignored) {
            owns = false;
        }
        return owns;
    }

    /**
     * Take an extension object from the package, as it is, without binding.
     * @param name The name of the extension
     * @return The extension object
     */
    private Phi extension(final String name) {
        final String fqn = String.join(".", this.pkg, name);
        if (!this.objects.containsKey(fqn)) {
            final Phi loaded = PhNest.load(new JavaPath(fqn).toString());
            loaded.put(Phi.RHO, this);
            this.objects.put(fqn, loaded);
        }
        return this.objects.get(fqn).copy();
    }

    /**
     * The object that shares its name with the package.
     * @return The object
     */
    private Phi object() {
        return this.origin.computeIfAbsent(
            this.pkg, key -> PhNest.load(new JavaPath(key).toString())
        );
    }

    /**
     * Instantiate a generated Java class by its name.
     * @param target The fully-qualified Java name
     * @return The object
     */
    private static Phi load(final String target) {
        try {
            return (Phi) Class.forName(target).getConstructor().newInstance();
        } catch (final ClassNotFoundException | NoSuchMethodException
            | InvocationTargetException | InstantiationException
            | IllegalAccessException ex) {
            throw new ExFailure(String.format("Can't instantiate the object '%s'", target), ex);
        }
    }
}
