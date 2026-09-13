/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import org.codehaus.plexus.util.SelectorUtils;

/**
 * A pattern that decides whether a walk takes a file.
 *
 * <p>The pattern arrives from a parameter in pom.xml and is matched the
 * way Maven itself matches one, through {@link SelectorUtils}, so that a
 * user writing {@code excludeSources} gets the rules every other plugin
 * reads. They are Ant patterns, where {@code **} stands for a run of
 * directories and a source at any depth is {@code **}{@code /*.eo}.</p>
 *
 * <p>The name is not {@code Glob} on purpose. Plexus resolves the name of
 * a configuration element to a class in this package, and {@code <glob>}
 * is what {@code <keepBinaries>} in {@code eo-runtime/pom.xml} calls its
 * members, so a class named {@code Glob} is taken for the type of those
 * members and the build dies on its missing no-argument constructor.</p>
 *
 * @since 0.73.4
 */
final class Globbed {

    /**
     * The pattern, as pom.xml writes it.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param pattern The Ant pattern
     */
    Globbed(final String pattern) {
        this.text = pattern;
    }

    /**
     * Does this pattern match the file?
     *
     * @param file The file, relative to the home of the walk
     * @return TRUE if it matches
     */
    boolean matches(final Path file) {
        return SelectorUtils.matchPath(this.text, file.toString());
    }
}
