/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.maven.model.Dependency;

/**
 * Every version legitimately resolved in a run, grouped by coordinate, so
 * {@link Resolving#cleanPlace} can tell a stale version (absent from the
 * whole set) from a sibling version resolved alongside it in the same run.
 *
 * @since 0.61.0
 */
final class ResolvedVersions {

    /**
     * Dependencies resolved in this run.
     */
    private final Collection<Dep> deps;

    /**
     * Ctor.
     *
     * @param resolved Dependencies resolved in this run
     */
    ResolvedVersions(final Collection<Dep> resolved) {
        this.deps = resolved;
    }

    /**
     * Every version, by coordinate.
     *
     * @return Versions, by coordinate
     */
    Map<String, Set<String>> byCoordinate() {
        final Map<String, Set<String>> result = new HashMap<>(0);
        for (final Dep dep : this.deps) {
            final Dependency dependency = dep.get();
            result.computeIfAbsent(
                new DepCoordinate(dependency).value(),
                key -> new HashSet<>(0)
            ).add(dependency.getVersion());
        }
        return result;
    }
}
