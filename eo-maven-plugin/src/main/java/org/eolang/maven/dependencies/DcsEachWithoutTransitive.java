/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.dependencies;

import java.util.Iterator;
import java.util.Objects;
import org.apache.maven.model.Dependency;
import org.cactoos.Func;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.eolang.maven.Coordinates;
import org.eolang.maven.ResolveMojo;

/**
 * Dependencies without transitive dependencies.
 *
 * @since 0.29.0
 */
public final class DcsEachWithoutTransitive implements Iterable<Dependency> {

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
    public DcsEachWithoutTransitive(
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
