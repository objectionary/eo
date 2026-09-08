/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Iterator;
import org.apache.maven.model.Dependency;
import org.cactoos.iterator.Filtered;

/**
 * Remove runtime dependency from the list of dependencies, if it is present there.
 * Useful for the "eo-runtime" module compilation.
 * @since 0.29
 */
final class DpsWithoutRuntime implements Dependencies {

    /**
     * All dependencies.
     */
    private final Dependencies delegate;

    /**
     * Constructor.
     * @param decoratee Dependencies delegate
     */
    DpsWithoutRuntime(final Dependencies decoratee) {
        this.delegate = decoratee;
    }

    @Override
    public Iterator<Dep> iterator() {
        return new Filtered<>(
            dep -> !DpsWithoutRuntime.isRuntime(dep.get()),
            this.delegate.iterator()
        );
    }

    // An artifact id is not unique in Maven, so the group has to be read
    // too: a dependency of somebody else named "eo-runtime" is not the
    // runtime this class removes, and dropping it would change the classpath
    // of a build that asked for nothing of the sort (#8147). This is the
    // same pair DpsWithRuntime decides by.
    private static boolean isRuntime(final Dependency dep) {
        return "org.eolang".equals(dep.getGroupId())
            && "eo-runtime".equals(dep.getArtifactId());
    }
}
