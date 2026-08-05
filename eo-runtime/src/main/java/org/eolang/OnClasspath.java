/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Cached check of whether a Java class is present on the classpath.
 *
 * <p>Package-extension dispatch probes the classpath with {@code Class.forName}
 * on every attribute miss (an inner loop of dataization), and the answer for a
 * given fully-qualified name never changes at runtime. This memoizes both the
 * positive and the negative answer, so a given name pays the lookup and the
 * caught {@link ClassNotFoundException} at most once.</p>
 *
 * @since 0.62
 */
final class OnClasspath {

    /**
     * Name to presence, cached across all lookups.
     */
    private static final Map<String, Boolean> CACHE = new ConcurrentHashMap<>(0);

    /**
     * EO name to presence, so that a name asked in EO notation pays the
     * translation to Java notation at most once.
     */
    private static final Map<String, Boolean> OBJECTS = new ConcurrentHashMap<>(0);

    private OnClasspath() {
    }

    /**
     * Is there a Java class with this name on the classpath?
     *
     * <p>The lookup neither links nor initializes the class it finds, since a
     * presence probe must not run anybody's code. It covers the classes of the
     * module this one belongs to, which for a classpath run means the classpath
     * itself; classes of the platform modules, like {@code java.util.List}, are
     * out of its reach and are reported as absent.</p>
     *
     * @param cls The fully-qualified Java name
     * @return TRUE if it exists
     */
    static boolean has(final String cls) {
        return OnClasspath.CACHE.computeIfAbsent(
            cls,
            name -> Objects.nonNull(Class.forName(OnClasspath.class.getModule(), name))
        );
    }

    /**
     * Is there an EO object with this name on the classpath?
     * @param fqn The fully-qualified EO name, like {@code Φ.string.as-number}
     * @return TRUE if it exists
     */
    static boolean object(final String fqn) {
        return OnClasspath.OBJECTS.computeIfAbsent(
            fqn, name -> OnClasspath.has(new JavaPath(name).toString())
        );
    }
}
