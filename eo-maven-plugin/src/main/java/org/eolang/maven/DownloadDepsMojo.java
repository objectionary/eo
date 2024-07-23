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
 * @since 0.38
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
    private static final Collection<Dependency> DEPS = new ListOf<>(
        new DepBuilder().withGroupId("net.java.dev.jna").withArtifactId("jna")
            .withVersion("5.14.0").withScope("compile").build()
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
        for (final Dependency dep : DownloadDepsMojo.DEPS) {
            this.central.accept(dep, this.classesDir.toPath());
        }
    }

    /**
     * Object for building {@link Dependency} inplace.
     *
     * @since 0.38
     */
    private static class DepBuilder {
        /**
         * Dependency.
         */
        private final Dependency dep;

        /**
         * Ctor.
         */
        DepBuilder() {
            this.dep = new Dependency();
        }

        /**
         * Set groupId.
         *
         * @param group The groupId
         * @return This object.
         */
        public DepBuilder withGroupId(final String group) {
            this.dep.setGroupId(group);
            return this;
        }

        /**
         * Set artifactId.
         *
         * @param artifact The artifactId
         * @return This object.
         */
        public DepBuilder withArtifactId(final String artifact) {
            this.dep.setArtifactId(artifact);
            return this;
        }

        /**
         * Set version.
         *
         * @param version The version.
         * @return This object.
         */
        public DepBuilder withVersion(final String version) {
            this.dep.setVersion(version);
            return this;
        }

        /**
         * Set scope.
         *
         * @param scope The scope.
         * @return This object.
         */
        public DepBuilder withScope(final String scope) {
            this.dep.setScope(scope);
            return this;
        }

        /**
         * Build dependency.
         *
         * @return Dependency instance.
         */
        public Dependency build() {
            return this.dep;
        }
    }
}
