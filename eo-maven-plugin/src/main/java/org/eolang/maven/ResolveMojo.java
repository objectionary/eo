/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import com.jcabi.xml.XMLDocument;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
import java.util.stream.Collectors;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Find all required runtime dependencies and add
 * them to classpath ("compile" scope).
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "resolve",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class ResolveMojo extends AbstractMojo {

    /**
     * From directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    private File targetDir;

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        final Path dir = this.targetDir.toPath().resolve("03-optimize");
        final Collection<Artifact> artifacts;
        try {
            artifacts = Files.walk(dir)
                .filter(file -> !file.toFile().isDirectory())
                .map(this::artifacts)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
        } catch (final IOException ex) {
            throw new MojoFailureException(
                String.format(
                    "Can't list XML files in %s",
                    dir
                ),
                ex
            );
        }
        for (final Artifact artifact : artifacts) {
            Logger.info(this, "%s needed", artifact);
        }
    }

    /**
     * Find the artifact required by this EO XML.
     *
     * @param file EO file
     * @return List of artifacts needed
     */
    private Collection<Artifact> artifacts(final Path file) {
        final Collection<Artifact> artifacts = new LinkedList<>();
        try {
            final String xpath = "//meta[head='rt' and part[1]='jvm']/part[2]";
            for (final String dep : new XMLDocument(file).xpath(xpath)) {
                artifacts.add(null);
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Can't pull %s into %s",
                    file, this.targetDir
                ),
                ex
            );
        }
        return artifacts;
    }
}
