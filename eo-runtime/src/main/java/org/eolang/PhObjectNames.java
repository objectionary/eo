/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

/**
 * EO object names by class resource path.
 * @since 1.0
 */
final class PhObjectNames {

    /**
     * Class file extension.
     */
    private static final String EXT = ".class";

    /**
     * Java root package.
     */
    private static final String ROOT = "org.eolang.";

    /**
     * Resource root package.
     */
    private static final String RROOT = "org/eolang/";

    /**
     * Ctor.
     */
    private PhObjectNames() {
    }

    /**
     * Object names by class resource path.
     * @param resource Resource path
     * @return EO object names
     */
    static Collection<String> names(final String resource) {
        final Collection<String> names;
        if (resource.startsWith(PhObjectNames.RROOT) && resource.endsWith(PhObjectNames.EXT)) {
            names = PhObjectNames.relative(
                resource.substring(
                    PhObjectNames.RROOT.length(),
                    resource.length() - PhObjectNames.EXT.length()
                )
            );
        } else {
            names = Collections.emptyList();
        }
        return names;
    }

    /**
     * Object names by relative class resource.
     * @param relative Relative class resource
     * @return EO object names
     */
    private static Collection<String> relative(final String relative) {
        final Collection<String> names;
        if (relative.endsWith("/package-info")) {
            names = PhObjectNames.packageInfo(relative);
        } else if (PhObjectNames.internal(relative)) {
            names = Collections.emptyList();
        } else {
            names = PhObjectNames.object(relative);
        }
        return names;
    }

    /**
     * Object names by package-info resource.
     * @param relative Relative class resource
     * @return EO object names
     */
    private static Collection<String> packageInfo(final String relative) {
        return PhObjectNames.singleton(
            PhJavaName.toObject(
                relative.substring(0, relative.length() - "/package-info".length())
            )
        );
    }

    /**
     * Object names by plain class resource.
     * @param relative Relative class resource
     * @return EO object names
     */
    private static Collection<String> object(final String relative) {
        final String object = PhObjectNames.annotated(relative);
        final Collection<String> names;
        if (object.isEmpty()) {
            names = PhObjectNames.fallback(relative);
        } else {
            names = Collections.singleton(object);
        }
        return names;
    }

    /**
     * Read annotated object name.
     * @param relative Relative class resource
     * @return EO object name or empty string
     */
    private static String annotated(final String relative) {
        String object = "";
        try {
            final XmirObject xmir = Class.forName(
                String.format(
                    "%s%s",
                    PhObjectNames.ROOT,
                    relative.replace('/', '.')
                ),
                false,
                Thread.currentThread().getContextClassLoader()
            ).getAnnotation(XmirObject.class);
            if (xmir != null) {
                object = PhJavaName.inPackage(
                    PhJavaName.parent(relative),
                    PhJavaName.original(xmir)
                );
            }
        } catch (final ClassNotFoundException | LinkageError | SecurityException ignored) {
            object = "";
        }
        return object;
    }

    /**
     * Fallback conversion from Java path to EO object.
     * @param relative Relative class resource
     * @return Object names
     */
    private static Collection<String> fallback(final String relative) {
        final Collection<String> names;
        final String object = PhJavaName.toObject(relative.replace('$', '/'));
        if (object.isEmpty() || object.endsWith("Test")) {
            names = Collections.emptyList();
        } else {
            names = Collections.singleton(object);
        }
        return names;
    }

    /**
     * Singleton object name.
     * @param object Object name
     * @return Collection
     */
    private static Collection<String> singleton(final String object) {
        final Collection<String> names;
        if (object.isEmpty()) {
            names = Collections.emptyList();
        } else {
            names = Collections.singleton(object);
        }
        return names;
    }

    /**
     * Check whether class resource points to an internal generated object.
     * @param relative Relative class resource
     * @return TRUE if object is internal
     */
    private static boolean internal(final String relative) {
        return Arrays.stream(relative.replace('$', '/').split("/"))
            .anyMatch(part -> part.startsWith("EOΦ") || part.startsWith("EOφ"));
    }
}
