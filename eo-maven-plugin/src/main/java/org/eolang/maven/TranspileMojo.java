/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrBulk;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.eolang.parser.ParsingTrain;

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
     * Extension for compiled sources in XMIR format (XML).
     */
    public static final String EXT = "xmir";

    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "06-transpile";

    /**
     * The directory where to put pre-transpile files.
     */
    public static final String PRE = "05-pre";

    /**
     * Parsing train with XSLs.
     */
    private static final Train<Shift> TRAIN = new TrBulk<>(
        new TrClasspath<>(new ParsingTrain().empty()),
        Arrays.asList(
            "/org/eolang/maven/pre/classes.xsl",
            "/org/eolang/maven/pre/package.xsl",
            "/org/eolang/maven/pre/junit.xsl",
            "/org/eolang/maven/pre/rename-junit-inners.xsl",
            "/org/eolang/maven/pre/attrs.xsl",
            "/org/eolang/maven/pre/varargs.xsl",
            "/org/eolang/maven/pre/data.xsl",
            "/org/eolang/maven/pre/to-java.xsl"
        )
    ).back().back();

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
        final Collection<Tojo> sources = this.tojos.value().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
                && row.get(AssembleMojo.ATTR_SCOPE).equals(this.scope)
        );
        int saved = 0;
        for (final Tojo tojo : sources) {
            final Path file = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2));
            final XML input = new XMLDocument(file);
            final String name = input.xpath("/program/@name").get(0);
            final Place place = new Place(name);
            final Path target = place.make(
                this.targetDir.toPath().resolve(TranspileMojo.DIR),
                TranspileMojo.EXT
            );
            final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_EO));
            if (
                target.toFile().exists()
                    && target.toFile().lastModified() >= file.toFile().lastModified()
                    && target.toFile().lastModified() >= src.toFile().lastModified()
            ) {
                Logger.info(
                    this, "XMIR %s (%s) were already transpiled to %s",
                    new Rel(file), name, new Rel(target)
                );
            } else {
                final List<Path> paths = this.transpile(src, input, target);
                for (final Path path : paths) {
                    this.transpiledTojos.value()
                        .add(String.valueOf(path))
                        .set(AssembleMojo.ATTR_XMIR2, tojo.get(AssembleMojo.ATTR_XMIR2));
                }
                saved += paths.size();
            }
        }
        Logger.info(
            this, "Transpiled %d XMIRs, created %d Java files in %s",
            sources.size(), saved, new Rel(this.generatedDir)
        );
        if (this.addSourcesRoot) {
            this.project.addCompileSourceRoot(this.generatedDir.getAbsolutePath());
            Logger.info(
                this, "The directory added to Maven 'compile-source-root': %s",
                new Rel(this.generatedDir)
            );
        }
        if (this.addTestSourcesRoot) {
            this.project.addTestCompileSourceRoot(this.generatedDir.getAbsolutePath());
            Logger.info(
                this, "The directory added to Maven 'test-compile-source-root': %s",
                new Rel(this.generatedDir)
            );
        }
    }

    /**
     * Transpile.
     * @param src The .eo file
     * @param input The .xmir file
     * @param target The path to transpiled .xmir file
     * @return List of Paths to generated java file
     * @throws IOException If any issues with I/O
     */
    private List<Path> transpile(final Path src, final XML input,
        final Path target) throws IOException {
        final String name = input.xpath("/program/@name").get(0);
        final int removed = this.removeTranspiled(src);
        if (removed > 0) {
            Logger.debug(
                this,
                "Removed %d Java files for %s",
                removed, new Rel(src)
            );
        } else {
            Logger.debug(
                this,
                "No Java files removed for %s",
                new Rel(src)
            );
        }
        final Place place = new Place(name);
        final Train<Shift> trn = new SpyTrain(
            TranspileMojo.TRAIN, place.make(
                this.targetDir.toPath().resolve(TranspileMojo.PRE),
                ""
            )
        );
        final XML out = new Xsline(trn).pass(input);
        final Path dir = this.targetDir.toPath().resolve(TranspileMojo.DIR);
        new Home(dir)
            .save(out.toString(), dir.relativize(target));
        return new JavaFiles(
            target,
            this.generatedDir.toPath()
        ).save();
    }

    /**
     * Remove transpiled files per EO.
     * @param src The eo path
     * @return Count of removed files
     */
    private int removeTranspiled(final Path src) {
        final Collection<Tojo> existed = this.tojos.value().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
                && row.get(AssembleMojo.ATTR_EO).equals(src.toString())
        );
        int count = 0;
        for (final Tojo exist : existed) {
            final List<Tojo> removable = this.transpiledTojos.value().select(
                row -> row.get(AssembleMojo.ATTR_XMIR2)
                    .equals(exist.get(AssembleMojo.ATTR_XMIR2))
            );
            for (final Tojo remove : removable) {
                final File file = new File(remove.get(Tojos.KEY));
                if (file.delete()) {
                    count += 1;
                }
            }
        }
        return count;
    }
}
