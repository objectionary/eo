/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A package object, coming from {@link Phi}.
 *
 * @since 0.22
 */
@SuppressWarnings("PMD.TooManyMethods")
final class PhPackage implements Phi {
    /**
     * Global package.
     * @checkstyle VisibilityModifierCheck (3 lines)
     * @checkstyle StaticVariableNameCheck (3 lines)
     */
    @SuppressWarnings("PMD.FieldNamingConventions")
    public static final String GLOBAL = "Φ";

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
    public boolean hasRho() {
        return true;
    }

    @Override
    public Phi take(final String name) {
        final String fqn = String.join(".", this.pkg, name);
        final Phi taken;
        if (name.equals(Attr.RHO)) {
            if (this.objects.containsKey(Attr.RHO)) {
                taken = this.objects.get(Attr.RHO);
            } else {
                throw new ExUnset(
                    String.format(
                        "The %s attribute is absent in package object '%s'",
                        Attr.RHO, this.pkg
                    )
                );
            }
        } else if (this.objects.containsKey(fqn)) {
            taken = this.objects.get(fqn).copy();
        } else if (name.contains(".")) {
            final String[] parts = name.split("\\.");
            Phi next = this.take(parts[0]);
            for (int idx = 1; idx < parts.length; ++idx) {
                next = next.take(parts[idx]);
            }
            taken = next;
        } else {
            final Phi loaded = this.loadPhi(fqn);
            loaded.put(Attr.RHO, this);
            this.put(fqn, loaded);
            taken = this.take(name);
        }
        return taken;
    }

    @Override
    public Phi take(final int pos) {
        throw new ExFailure(
            "Can't #take(#d) from package object \"%s\"", pos, this.pkg
        );
    }

    @Override
    public void put(final int pos, final Phi object) {
        throw new ExFailure(
            "Can't #put(%d, %s) to package object \"%s\"", pos, object, this.pkg
        );
    }

    @Override
    public void put(final String name, final Phi object) {
        this.objects.put(name, object);
    }

    @Override
    public byte[] delta() {
        throw new ExFailure("Can't take #data() from package object \"%s\"", this.pkg);
    }

    /**
     * Load phi object by package name from ClassLoader.
     * @param fqn FQN of the EO object
     * @return Phi
     */
    @SuppressWarnings("PMD.PreserveStackTrace")
    private Phi loadPhi(final String fqn) {
        final String target = new JavaPath(fqn).toString();
        Phi loaded;
        try {
            Class.forName(String.format("%s.package-info", target));
            loaded = new PhPackage(fqn);
        } catch (final ClassNotFoundException pckg) {
            try {
                loaded = (Phi) Class.forName(target)
                    .getConstructor()
                    .newInstance();
            } catch (final ClassNotFoundException phi) {
                throw new ExFailure(
                    String.format(
                        "Couldn't find object '%s' because there's no class or package '%s'",
                        fqn, target
                    ),
                    phi
                );
            } catch (final NoSuchMethodException
                | InvocationTargetException
                | InstantiationException
                | IllegalAccessException ex
            ) {
                throw new ExFailure(
                    String.format(
                        "Couldn't build Java object \"%s\" in EO package \"%s\"",
                        target, this.pkg
                    ),
                    ex
                );
            }
        }
        return loaded;
    }
}
