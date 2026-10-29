/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;

/**
 * The few names of the program the engine has to know.
 *
 * <ul>
 *   <li>The canon (design doc 3.1): the kinds that are one shared copy
 *   each, born {@code canon}. The first holds the data, and every other
 *   one holds the first: {@code Φ.bytes}, then {@code Φ.number} and
 *   {@code Φ.string}.</li>
 *   <li>The roots of list code (3.6): every formation under one of these
 *   packages is list code, and a tag starts where other code makes a copy
 *   of it: {@code Φ.tuple}, {@code Φ.map} and {@code Φ.set}.</li>
 *   <li>The scalars (3.6): the packages whose formations give back new
 *   data and take no tag: {@code Φ.number}, {@code Φ.bytes} and so
 *   on.</li>
 * </ul>
 *
 * @since 0.76.0
 */
public final class Names {

    /**
     * The canon, the one that holds the data first.
     */
    private final List<String> canon;

    /**
     * The packages of list code.
     */
    private final List<String> roots;

    /**
     * The packages that take no tag.
     */
    private final List<String> scalars;

    /**
     * Ctor, with the names of eo-runtime.
     */
    public Names() {
        this(
            List.of("Φ.bytes", "Φ.number", "Φ.string"),
            List.of("Φ.tuple", "Φ.map", "Φ.set"),
            List.of(
                "Φ.number", "Φ.bytes", "Φ.string", "Φ.bool", "Φ.true", "Φ.false",
                "Φ.i8", "Φ.i16", "Φ.i32", "Φ.i64", "Φ.u8", "Φ.u16", "Φ.u32", "Φ.u64",
                "Φ.dataized"
            )
        );
    }

    /**
     * Ctor.
     *
     * @param kinds The canon, the one that holds the data first
     * @param lists The packages of list code
     * @param plain The packages that take no tag
     */
    public Names(final List<String> kinds, final List<String> lists, final List<String> plain) {
        this.canon = kinds;
        this.roots = lists;
        this.scalars = plain;
    }

    /**
     * The canon, the one that holds the data first.
     *
     * @return The names of the formations
     */
    List<String> canon() {
        return this.canon;
    }

    /**
     * Whether a package is one of list code.
     *
     * @param pkg The package, as {@code Φ.tuple}
     * @return TRUE if it is
     */
    boolean root(final String pkg) {
        return this.roots.contains(pkg);
    }

    /**
     * Whether a formation is in a package that takes no tag.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean scalar(final String form) {
        final String dotted = form + ".";
        boolean found = false;
        for (final String pkg : this.scalars) {
            if (dotted.startsWith(pkg + ".")) {
                found = true;
                break;
            }
        }
        return found;
    }
}
