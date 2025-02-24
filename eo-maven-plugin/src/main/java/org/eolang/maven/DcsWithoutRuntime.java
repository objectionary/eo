/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Iterator;
import org.apache.maven.model.Dependency;
import org.cactoos.iterator.Filtered;

/**
 * Remove runtime dependency from the list of dependencies, if it is present there.
 * Useful for the "eo-runtime" module compilation.
 *
 * @since 0.29
 */
final class DcsWithoutRuntime implements Iterable<Dependency> {

    /**
     * All dependencies.
     */
    private final Iterable<? extends Dependency> delegate;

    /**
     * Constructor.
     * @param decoratee Dependencies delegate.
     */
    DcsWithoutRuntime(final Iterable<? extends Dependency> decoratee) {
        this.delegate = decoratee;
    }

    @Override
    public Iterator<Dependency> iterator() {
        return new Filtered<>(
            dep -> !"eo-runtime".equals(dep.getArtifactId()),
            this.delegate.iterator()
        );
    }
}
