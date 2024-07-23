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
package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
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
    private static final Collection<DepSupplier> DEPS = new ListOf<>(
        new DepSupplier("net.java.dev.jna", "jna", "5.14.0")
    );

    /**
     * Directory where classes are stored in target.
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        defaultValue = "${project.build.directory}/classes",
        readonly = true,
        required = true
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File classesDir;

    /**
     * The central.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private BiConsumer<Dependency, Path> central;

    @Override
    void exec() throws IOException {
        if (this.central == null) {
            this.central = new Central(this.project, this.session, this.manager);
        }
        for (final DepSupplier dep : DownloadDepsMojo.DEPS) {
            this.central.accept(dep.take(), this.classesDir.toPath());
        }
    }

    /**
     * Object for lazy building {@link Dependency} inplace.
     *
     * @since 0.39
     */
    private static class DepSupplier {
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
        DepSupplier(final String group, final String artifact, final String version) {
            this.group = group;
            this.artifact = artifact;
            this.version = version;
        }

        /**
         * Makes dependency and returns it.
         *
         * @return Dependency instance.
         */
        public Dependency take() {
            final Dependency dep = new Dependency();
            dep.setGroupId(this.group);
            dep.setArtifactId(this.artifact);
            dep.setVersion(this.version);
            return dep;
        }
    }
}
