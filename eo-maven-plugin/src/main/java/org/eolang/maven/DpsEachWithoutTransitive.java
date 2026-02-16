/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
final class DpsEachWithoutTransitive implements Dependencies {

    /**
     * Original dependencies.
     */
    private final Dependencies delegate;

    /**
     * Strategy to get transitive dependencies for a particular dependency.
     */
    private final Func<Dep, Dependencies> transitive;

    /**
     * Ctor.
     * @param dependencies Dependencies
     * @param strategy Strategy
     */
    DpsEachWithoutTransitive(
        final Dependencies dependencies,
        final Func<Dep, Dependencies> strategy
    ) {
        this.delegate = dependencies;
        this.transitive = strategy;
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public Iterator<Dep> iterator() {
        return new Mapped<>(
            dependency -> {
                final Iterable<Dep> transitives = new Filtered<>(
                    dep -> {
                        final Dependency dpndncy = dep.get();
                        final Dependency orig = dependency.get();
                        return !DpsEachWithoutTransitive.eqTo(dpndncy, orig)
                            && DpsEachWithoutTransitive.required(dpndncy)
                            && !MjResolve.isRuntime(dpndncy);
                    },
                    this.transitive.apply(dependency)
                );
                final String list = String.join(
                    ", ",
                    new Mapped<>(Dep::toString, transitives)
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
    private static boolean required(final Dependency dep) {
        return dep.getScope() == null
            || dep.getScope().isEmpty()
            || "runtime".equals(dep.getScope())
            || "compiled".equals(dep.getScope());
    }
}
