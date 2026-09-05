/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A package object, coming from {@link Phi}.
 *
 * <p>A package is a namespace, so its attributes are the objects the
 * package holds and nothing else: the only attribute a caller may bind
 * into one is {@link Phi#RHO}, the receiver a dispatch sets. Everything
 * else a package answers it loads by name, caching it under the fully
 * qualified name {@link #take(String)} looks it up by, which is not the
 * name the caller writes — so an object bound under a plain attribute
 * name could never be handed back, and {@link #put(String, Phi)} says so
 * rather than storing it out of reach.</p>
 *
 * @since 0.22
 */
final class PhPackage implements Phi {

    /**
     * Global package.
     */
    static final String GLOBAL = "Φ";

    /**
     * The name of the Java package.
     */
    private final String pkg;

    /**
     * Objects in the package.
     */
    private final Map<String, Phi> objects;

    /**
     * Ctor.
     * @param name The name
     */
    PhPackage(final String name) {
        this.pkg = name;
        this.objects = new ConcurrentHashMap<>(0);
    }

    @Override
    public String locator() {
        return "?:?:?";
    }

    @Override
    public String forma() {
        return this.pkg;
    }

    @Override
    public Phi copy() {
        return this;
    }

    @Override
    public boolean needsRho() {
        return false;
    }

    @Override
    public Phi take(final String name) {
        final String fqn = String.join(".", this.pkg, name);
        final Phi taken;
        if (name.equals(Phi.RHO)) {
            if (this.objects.containsKey(Phi.RHO)) {
                taken = this.objects.get(Phi.RHO);
            } else {
                throw new ExUnset(
                    String.format(
                        "The %s attribute is absent in package object '%s'",
                        Phi.RHO, this.pkg
                    )
                );
            }
        } else if (this.objects.containsKey(fqn)) {
            taken = this.objects.get(fqn).copy();
        } else if (name.contains(".")) {
            final String[] parts = name.split("\\.", -1);
            Phi next = this.take(parts[0]);
            for (int idx = 1; idx < parts.length; ++idx) {
                next = next.take(parts[idx]);
            }
            taken = next;
        } else {
            taken = this.objects.computeIfAbsent(fqn, this::bound).copy();
        }
        return taken;
    }

    @Override
    public void put(final int pos, final Phi object) {
        throw new ExFailure(
            "Can't #put(%d, %s) to package object \"%s\"", pos, object, this.pkg
        );
    }

    @Override
    public void put(final String name, final Phi object) {
        if (!name.equals(Phi.RHO)) {
            throw new ExFailure(
                "Can't #put(\"%s\", %s) to package object \"%s\", only %s is accepted",
                name, object, this.pkg, Phi.RHO
            );
        }
        this.objects.put(name, object);
    }

    @Override
    public byte[] delta() {
        throw new ExFailure("Can't take #data() from package object \"%s\"", this.pkg);
    }

    @Override
    public Phi normalized() {
        return this;
    }

    @Override
    public String φTerm() {
        return this.pkg;
    }

    private Phi bound(final String fqn) {
        final Phi loaded = this.loadPhi(fqn);
        if (loaded.needsRho()) {
            loaded.put(Phi.RHO, this);
        }
        return loaded;
    }

    private Phi loadPhi(final String fqn) {
        final String target = new JavaPath(fqn).toString();
        final String pinfo = String.format("%s.package-info", new JavaPath(fqn).pkg());
        Phi loaded;
        try {
            loaded = Class.forName(target)
                .asSubclass(Phi.class)
                .getConstructor()
                .newInstance();
            if (loaded instanceof Pure) {
                loaded = new PhSticky(loaded);
            }
        } catch (final ClassNotFoundException phi) {
            try {
                Class.forName(pinfo);
                loaded = new PhPackage(fqn);
            } catch (final ClassNotFoundException pckg) {
                final ExFailure failure = new ExFailure(
                    String.format(
                        "Couldn't find object '%s' because there's no class '%s' or package-info class: '%s', at least one of them must exist%s",
                        fqn, target, pinfo,
                        new ObjectSuggestions(
                            Thread.currentThread().getContextClassLoader()
                        ).suggest(fqn)
                    ),
                    pckg
                );
                failure.addSuppressed(phi);
                throw failure;
            }
        } catch (final NoSuchMethodException | InvocationTargetException
            | InstantiationException | IllegalAccessException ex
        ) {
            throw new ExFailure(
                String.format(
                    "Couldn't build Java object \"%s\" in EO package \"%s\"",
                    target, this.pkg
                ),
                ex
            );
        }
        return loaded;
    }
}
