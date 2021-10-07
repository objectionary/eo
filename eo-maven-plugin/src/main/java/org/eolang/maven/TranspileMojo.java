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
import java.nio.file.Paths;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.eolang.tojos.MonoTojos;
import org.eolang.tojos.Tojo;
import org.eolang.tojos.Tojos;

/**
 * Compile.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 */
@Mojo(
    name = "transpile",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.LongVariable")
public final class TranspileMojo extends SafeMojo {

    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "06-transpile";

    /**
     * The directory where to put pre-transpile files.
     */
    public static final String PRE = "05-pre";

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/generated-sources"
    )
    private File generatedDir;

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
    @Parameter(property = "compiler", defaultValue = "canonical")
    @SuppressWarnings("PMD.ImmutableField")
    private String compiler;

    @Override
    public void exec() throws IOException {
        final Transpiler cmp;
        if ("canonical".equals(this.compiler)) {
            cmp = new TranspilerCanonical(
                this.targetDir.toPath().resolve(TranspileMojo.DIR),
                this.targetDir.toPath().resolve(TranspileMojo.PRE)
            );
        } else {
            cmp = new TranspilerAlternative(this.compiler);
        }
        final Tojos tojos = new MonoTojos(this.foreign);
        final Collection<Tojo> sources = tojos.select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
        );
        int total = 0;
        for (final Tojo tojo : sources) {
            final int done = cmp.transpile(
                Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2)),
                this.generatedDir.toPath()
            );
            total += done;
        }
        if (total > 0) {
            if (this.addSourcesRoot) {
                this.project.addCompileSourceRoot(
                    this.generatedDir.getAbsolutePath()
                );
                Logger.info(
                    this, "The directory added to transpile-source-root: %s",
                    this.generatedDir
                );
            }
            if (this.addTestSourcesRoot) {
                this.project.addTestCompileSourceRoot(
                    this.generatedDir.getAbsolutePath()
                );
                Logger.info(
                    this, "The directory added to test-transpile-source-root: %s",
                    this.generatedDir
                );
            }
        }
    }

}
