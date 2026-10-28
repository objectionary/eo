/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.HashSet;

/**
 * The locators naming a package and nothing besides it.
 *
 * <p>A {@code +package foo} puts the objects of its file under {@code Φ.foo}
 * and forms nothing at the prefix itself, so {@code Φ.foo} names no object
 * when no file holds a {@code foo} beside the package. The prefix is still
 * read: {@code Q.foo.oak} is a dispatch {@code .oak} off {@code Φ.foo}, and
 * every {@code +alias foo.oak} is parsed the same way. Left unanswered, the
 * dispatch has no bearer and the whole chain behind it stays unknown
 * (#9358).</p>
 *
 * <p>A prefix something else already names is not here. {@code Φ.string} is
 * a package of the runtime and an object of its own, and the row of that
 * object is the better answer, since it says what the object has.</p>
 *
 * @since 0.77.0
 */
final class Packages {

    /**
     * The locator of everything the program names.
     */
    private final Collection<String> named;

    /**
     * Ctor.
     *
     * @param locators The locator of everything the program names
     */
    Packages(final Collection<String> locators) {
        this.named = locators;
    }

    /**
     * The prefixes naming a package and nothing besides it.
     *
     * @return The locators, each of them the prefix of a name and the name
     *  of nothing
     */
    Collection<String> all() {
        final Collection<String> taken = new HashSet<>(this.named);
        final Collection<String> found = new HashSet<>(0);
        for (final String locator : taken) {
            int dot = locator.indexOf('.');
            while (dot > 0) {
                final String prefix = locator.substring(0, dot);
                if (!taken.contains(prefix)) {
                    found.add(prefix);
                }
                dot = locator.indexOf('.', dot + 1);
            }
        }
        return found;
    }
}
