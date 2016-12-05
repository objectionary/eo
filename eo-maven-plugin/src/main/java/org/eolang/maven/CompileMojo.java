/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.eolang.compiler.Program;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Compile.
 *
 * @author Yegor Bugayenko (yegor256@gmail.com)
 * @version $Id$
 * @since 0.1
 */
@Mojo(
    name = "compile",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class CompileMojo extends AbstractMojo {

    /**
     * Maven project.
     */
    @Parameter(defaultValue = "${project}")
    private MavenProject project;

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = false,
        readonly = false,
        defaultValue = "${project.build.directory}/generated-sources/eo"
    )
    private transient File targetDirectory;

    /**
     * Directory in which .eo files are located.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = false,
        readonly = false,
        defaultValue = "${project.basedir}/src/main/eo"
    )
    private transient File sourceDirectory;

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        if (this.targetDirectory.mkdirs()) {
            Logger.info(this, "Directory created: %s", this.targetDirectory);
        }
        try {
            Files.list(this.sourceDirectory.toPath()).forEach(this::compile);
        } catch (final IOException ex) {
            throw new MojoFailureException(
                String.format(
                    "Can't list EO files in %s",
                    this.sourceDirectory
                ),
                ex
            );
        }
        this.project.addCompileSourceRoot(
            this.targetDirectory.getAbsolutePath()
        );
        Logger.info(
            this, "Directory added to sources: %s",
            this.targetDirectory
        );
    }

    /**
     * Compile one EO file.
     * @param file EO file
     */
    private void compile(final Path file) {
        try {
            new Program(new String(Files.readAllBytes(file))).save(
                this.targetDirectory.toPath()
            );
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Can't compile %s into %s",
                    file, this.targetDirectory
                ),
                ex
            );
        }
        Logger.info(this, "%s compiled to %s", file, this.targetDirectory);
    }

}
