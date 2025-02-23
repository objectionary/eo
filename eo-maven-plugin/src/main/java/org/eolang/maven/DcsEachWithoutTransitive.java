/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Iterator;
import java.util.Objects;
import org.apache.maven.model.Dependency;
import org.cactoos.Func;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;

/**
 * Dependencies without transitive dependencies.
 *
 * @since 0.29.0
 */
final class DcsEachWithoutTransitive implements Iterable<Dependency> {

    /**
     * Original dependencies.
     */
    private final Iterable<Dependency> delegate;

    /**
     * Strategy to get transitive dependencies for a particular dependency.
     */
    private final Func<? super Dependency, ? extends Iterable<Dependency>> transitive;

    /**
     * Ctor.
     * @param dependencies Dependencies
     * @param strategy Strategy
     */
    DcsEachWithoutTransitive(
        final Iterable<Dependency> dependencies,
        final Func<? super Dependency, ? extends Iterable<Dependency>> strategy
    ) {
        this.delegate = dependencies;
        this.transitive = strategy;
    }

    @Override
    public Iterator<Dependency> iterator() {
        return new Mapped<>(
            dependency -> {
                final Iterable<Dependency> transitives = new Filtered<>(
                    dep -> !DcsEachWithoutTransitive.eqTo(dep, dependency)
                        && DcsEachWithoutTransitive.isRuntimeRequired(dep)
                        && !ResolveMojo.isRuntime(dep),
                    this.transitive.apply(dependency)
                );
                final String list = String.join(
                    ", ",
                    new Mapped<>(
                        dep -> new Coordinates(dep).toString(),
                        transitives
                    )
                );
                if (!list.isEmpty()) {
                    throw new IllegalStateException(
                        String.format(
                            "%s contains transitive dependencies: [%s]",
                            dependency, list
                        )
                    );
                }
                return dependency;
            },
            this.delegate
        ).iterator();
    }

    /**
     * Compare with NULL-safety.
     * @param left Left
     * @param right Right
     * @return TRUE if they are equal
     */
    private static boolean eqTo(final Dependency left, final Dependency right) {
        return Objects.equals(
            Objects.toString(left.getClassifier(), ""),
            Objects.toString(right.getClassifier(), "")
        )
            && Objects.equals(left.getArtifactId(), right.getArtifactId())
            && Objects.equals(left.getGroupId(), right.getGroupId());
    }

    /**
     * Check if dependency is not needed at runtime.
     * @param dep Maven dependency
     * @return True if it's not needed at runtime
     */
    private static boolean isRuntimeRequired(final Dependency dep) {
        return dep.getScope() == null
            || dep.getScope().isEmpty()
            || "runtime".equals(dep.getScope())
            || "compiled".equals(dep.getScope());
    }
}
