/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
        final String obj = this.eoPackage(name);
        final String key = new JavaPath(obj).toString();
        return this.objects.computeIfAbsent(
            key,
            k -> {
                final Phi initialized = this.loadPhi(key, obj);
                if (!(initialized instanceof PhPackage)) {
                    initialized.put(Attr.RHO, this);
                }
                return initialized;
            }
        ).copy();
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
        throw new ExFailure(
            "Can't #put(%s, %s) to package object \"%s\"", name, object, this.pkg
        );
    }

    @Override
    public byte[] delta() {
        throw new ExFailure("Can't take #data() from package object \"%s\"", this.pkg);
    }

    /**
     * Creates eo-package path by name.
     * @param name The name of an en object.
     * @return Eo-package path.
     */
    private String eoPackage(final String name) {
        return String.join(".", this.pkg, name);
    }

    /**
     * Load phi object by package name from ClassLoader.
     * @param path Path to directory or .java file
     * @param object Object FQN
     * @return Phi
     */
    private Phi loadPhi(final String path, final String object) {
        final Path pth = Paths.get("target/classes").resolve(path.replace(".", File.separator));
        final Phi phi;
        if (Files.exists(pth) && Files.isDirectory(pth)) {
            phi = new PhPackage(object);
        } else {
            final Path clazz = Paths.get(String.format("%s.class", pth));
            if (!Files.exists(clazz) || Files.isDirectory(clazz)) {
                throw new ExFailure(
                    String.format("Couldn't find object '%s'", object)
                );
            }
            try {
                phi = (Phi) Class.forName(path)
                    .getConstructor()
                    .newInstance();
            } catch (final ClassNotFoundException
                | NoSuchMethodException
                | InvocationTargetException
                | InstantiationException
                | IllegalAccessException ex
            ) {
                throw new ExFailure(
                    String.format(
                        "Couldn't build Java object \"%s\" in EO package \"%s\"",
                        path, this.pkg
                    ),
                    ex
                );
            }
        }
        return phi;
    }
}
