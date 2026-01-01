/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
import java.util.Collection;
import java.util.Iterator;
import org.cactoos.list.ListOf;

/**
 * Dependencies together with offline EO runtime dependency.
 * @since 0.56.5
 */
final class DpsOfflineRuntime implements Dependencies {

    /**
     * EO current offline version.
     */
    private static final Dep EO_OFFLINE = new Dep().withGroupId("org.eolang")
        .withArtifactId("eo-runtime")
        .withVersion(Manifests.read("EO-Version"));

    /**
     * All dependencies.
     */
    private final Iterable<Dep> all;

    /**
     * Ctor.
     * @param dlg All dependencies
     */
    DpsOfflineRuntime(final Iterable<Dep> dlg) {
        this.all = dlg;
    }

    @Override
    public Iterator<Dep> iterator() {
        final Collection<Dep> deps = new ListOf<>(this.all);
        final boolean present = deps.stream().anyMatch(
            dep -> dep.toString().startsWith("org.eolang:eo-runtime:")
        );
        if (!present) {
            deps.add(DpsOfflineRuntime.EO_OFFLINE);
        }
        return deps.iterator();
    }
}
