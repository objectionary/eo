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

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.list.ListOf;

@Mojo(
    name = "jna",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class DownloadDepsMojo extends SafeMojo {

    @Parameter(defaultValue = "${project.build.directory}", readonly = true, required = true)
    private File jnaOutputDir;

    /**
     * The central.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private BiConsumer<Dependency, Path> central;

    private final Collection<Dependency> deps = new ListOf<>(
        new DepBuilder(
            "net.java.dev.jna",
            "jna",
            "5.14.0"
        ).withScope("compile").build()
    );

    @Override
    void exec() throws IOException {
        if (this.central == null) {
            this.central = new Central(this.project, this.session, this.manager);
        }
        Path outputPath = Paths.get(this.jnaOutputDir.getPath(), "classes");
        for (final Dependency dep: deps) {
            this.central.accept(dep, outputPath);
            Logger.info(
                this,
                "Unpacked dependency with groupId %s into directory %s",
                dep.getGroupId(),
                outputPath
            );
        }
    }

    static class DepBuilder {
        private final Dependency dep;

        DepBuilder() {
            this.dep = new Dependency();
        }

        DepBuilder(final String group, final String artifact, final String version) {
            this.dep = new Dependency();
            this.dep.setGroupId(group);
            this.dep.setArtifactId(artifact);
            this.dep.setVersion(version);
        }

        public DepBuilder withGroupId(final String group) {
            this.dep.setGroupId(group);
            return this;
        }

        public DepBuilder withArtifactId(final String artifact) {
            this.dep.setArtifactId(artifact);
            return this;
        }

        public DepBuilder withVersion(final String version) {
            this.dep.setVersion(version);
            return this;
        }

        public DepBuilder withScope(final String scope) {
            this.dep.setScope(scope);
            return this;
        }

        public Dependency build() {
            return this.dep;
        }
    }
}
