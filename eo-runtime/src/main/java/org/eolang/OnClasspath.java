/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Map;
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

    private OnClasspath() {
    }

    /**
     * Is there a Java class with this name on the classpath?
     * @param cls The fully-qualified Java name
     * @return TRUE if it exists
     */
    static boolean has(final String cls) {
        return OnClasspath.CACHE.computeIfAbsent(cls, OnClasspath::probe);
    }

    /**
     * Probe the classpath for a class, uncached.
     * @param cls The fully-qualified Java name
     * @return TRUE if it exists
     */
    private static boolean probe(final String cls) {
        boolean found;
        try {
            Class.forName(cls);
            found = true;
        } catch (final ClassNotFoundException ex) {
            found = false;
        }
        return found;
    }
}
