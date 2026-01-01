/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Iterator;
import org.cactoos.iterator.Filtered;

/**
 * Remove runtime dependency from the list of dependencies, if it is present there.
 * Useful for the "eo-runtime" module compilation.
 *
 * @since 0.29
 */
final class DpsWithoutRuntime implements Dependencies {

    /**
     * All dependencies.
     */
    private final Dependencies delegate;

    /**
     * Constructor.
     * @param decoratee Dependencies delegate.
     */
    DpsWithoutRuntime(final Dependencies decoratee) {
        this.delegate = decoratee;
    }

    @Override
    public Iterator<Dep> iterator() {
        return new Filtered<>(
            dep -> !"eo-runtime".equals(dep.get().getArtifactId()),
            this.delegate.iterator()
        );
    }
}
