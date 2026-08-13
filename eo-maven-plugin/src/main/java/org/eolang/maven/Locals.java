/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * The objects that belong to a package, as the compiler knows them.
 *
 * <p>A bare reference in a program carrying a {@code +package} meta is
 * homed into that package only when the qualified name is one of these,
 * which is what {@code add-default-package.xsl} reads. {@link Parsing}
 * tells it, and {@link MjFormat} has to tell {@code unhome-package.xsl}
 * the very same thing, or the two would disagree on which references were
 * homed at all.</p>
 *
 * @since 0.62.0
 */
final class Locals {

    /**
     * The sources of this build.
     */
    private final Collection<TjForeign> sources;

    /**
     * Ctor.
     * @param srcs The sources of this build
     */
    Locals(final Collection<TjForeign> srcs) {
        this.sources = srcs;
    }

    /**
     * Their qualified names, space separated.
     * @return The names, sorted, with no duplicates
     */
    String names() {
        return this.sources.stream()
            .map(TjForeign::identifier)
            .filter(id -> id.contains("."))
            .distinct()
            .sorted()
            .collect(Collectors.joining(" "));
    }
}
