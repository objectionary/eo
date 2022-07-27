/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
import com.yegor256.tojos.Tojo;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;

/**
 * Compile.
 *
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
        property = "eo.generatedDir",
        required = true,
        defaultValue = "${project.build.directory}/generated-sources"
    )
    private File generatedDir;

    /**
     * Whether we should fail on warn.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.failOnWarning",
        required = true,
        defaultValue = "false"
    )
    private boolean failOnWarning;

    /**
     * Whether we should fail on error.
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.23.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(
        property = "eo.failOnError",
        defaultValue = "true")
    private boolean failOnError = true;

    /**
     * Add to source root.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.addSourcesRoot")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean addSourcesRoot = true;

    /**
     * Add to test source root.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.addTestSourcesRoot")
    private boolean addTestSourcesRoot;

    @Override
    public void exec() throws IOException {
        final Transpiler cmp = new Transpiler(
            this.targetDir.toPath().resolve(TranspileMojo.DIR),
            this.targetDir.toPath().resolve(TranspileMojo.PRE)
        );
        final Collection<Tojo> sources = this.tojos.value().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
                && row.get(AssembleMojo.ATTR_SCOPE).equals(this.scope)
        );
        int saved = 0;
        for (final Tojo tojo : sources) {
            final Path transpiled = cmp.transpile(
                Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2))
            );
            final Set<String> failures = new HashSet<>(3);
            if (this.failOnWarning) {
                failures.add(Sanitized.WARNING);
            }
            if (this.failOnError) {
                failures.add(Sanitized.ERROR);
            }
            new Sanitized(transpiled).sanitize(failures);
            saved += new JavaFiles(
                transpiled,
                this.generatedDir.toPath()
            ).save();
        }
        Logger.info(
            this, "Transpiled %d XMIRs, created %d Java files in %s",
            sources.size(), saved, Save.rel(this.generatedDir.toPath())
        );
        if (this.addSourcesRoot) {
            this.project.addCompileSourceRoot(
                this.generatedDir.getAbsolutePath()
            );
            Logger.info(
                this, "The directory added to Maven 'compile-source-root': %s",
                Save.rel(this.generatedDir.toPath())
            );
        }
        if (this.addTestSourcesRoot) {
            this.project.addTestCompileSourceRoot(
                this.generatedDir.getAbsolutePath()
            );
            Logger.info(
                this, "The directory added to Maven 'test-compile-source-root': %s",
                Save.rel(this.generatedDir.toPath())
            );
        }
    }
}
