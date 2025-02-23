/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.Collection;
import java.util.function.Supplier;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.list.ListOf;

/**
 * Downloads dependencies.
 *
 * @since 0.39
 */
@Mojo(
    name = "deps",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class DownloadDepsMojo extends SafeMojo {

    /**
     * Dependencies to download.
     */
    private static final Collection<Supplier<Dependency>> DEPS = new ListOf<>(
        new DepFunc("net.java.dev.jna", "jna", "5.14.0")
    );

    @Override
    void exec() throws IOException {
        for (final Supplier<Dependency> dep : DownloadDepsMojo.DEPS) {
            this.central.accept(dep.get(), this.classesDir.toPath());
        }
    }

    /**
     * Object for lazy building {@link Dependency} inplace.
     *
     * @since 0.39
     */
    private static class DepFunc implements Supplier<Dependency> {
        /**
         * The groupId.
         */
        private final String group;

        /**
         * The artifactId.
         */
        private final String artifact;

        /**
         * The version.
         */
        private final String version;

        /**
         * Ctor.
         *
         * @param group The groupId.
         * @param artifact The artifactId.
         * @param version The version.
         */
        DepFunc(final String group, final String artifact, final String version) {
            this.group = group;
            this.artifact = artifact;
            this.version = version;
        }

        @Override
        public Dependency get() {
            final Dependency dep = new Dependency();
            dep.setGroupId(this.group);
            dep.setArtifactId(this.artifact);
            dep.setVersion(this.version);
            return dep;
        }
    }
}
