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
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Compile.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 */
@Mojo(
    name = "compile",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.LongVariable")
public final class CompileMojo extends AbstractMojo {

    /**
     * The directory where to compile to.
     */
    public static final String DIR = "06-compile";

    /**
     * The directory where to put pre-compile files.
     */
    public static final String PRE = "05-pre";

    /**
     * Maven project.
     */
    @Parameter(defaultValue = "${project}")
    private MavenProject project;

    /**
     * Target directory.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/generated-sources"
    )
    private File generatedDir;

    /**
     * Target directory.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    private File targetDir;

    /**
     * Add to source root.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    @SuppressWarnings("PMD.ImmutableField")
    private boolean addSourcesRoot = true;

    /**
     * Add to test source root.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private boolean addTestSourcesRoot;

    /**
     * Which compiler to use: original or HSE.
     */
    @Parameter(
        property = "compiler"
    )
    @SuppressWarnings("PMD.ImmutableField")
    private String compiler;

    @Override
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        if (this.generatedDir.mkdirs()) {
            Logger.info(this, "Gen directory created: %s", this.generatedDir);
        }
        final Path dir = this.targetDir.toPath().resolve(OptimizeMojo.DIR);
        final Compiler cmp;
        if (this.compiler == null) {
            cmp = new CompilerOriginal(
                this.targetDir.toPath().resolve(CompileMojo.DIR),
                this.targetDir.toPath().resolve(CompileMojo.PRE)
            );
        } else {
            cmp = new CompilerAlternative(this.compiler);
        }
        new Walk(dir).forEach(
            file -> {
                try {
                    cmp.compile(file, this.generatedDir.toPath());
                } catch (final IOException ex) {
                    throw new IllegalStateException(
                        String.format(
                            "Failed to compile %s to %s",
                            file, this.generatedDir
                        ),
                        ex
                    );
                }
            }
        );
        if (this.addSourcesRoot) {
            this.project.addCompileSourceRoot(
                this.generatedDir.getAbsolutePath()
            );
            Logger.info(
                this, "The directory added to compile-source-root: %s",
                this.generatedDir
            );
        }
        if (this.addTestSourcesRoot) {
            this.project.addTestCompileSourceRoot(
                this.generatedDir.getAbsolutePath()
            );
            Logger.info(
                this, "The directory added to test-compile-source-root: %s",
                this.generatedDir
            );
        }
    }

}
