/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Rel;
import org.eolang.parser.ParsingTrain;
import org.eolang.parser.StUnhex;

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
    public static final String DIR = "6-transpile";

    /**
     * Extension for compiled sources in XMIR format (XML).
     */
    static final String EXT = "xmir";

    /**
     * Parsing train with XSLs.
     */
    static final Train<Shift> TRAIN = new TrJoined<>(
        new TrClasspath<>(
            new ParsingTrain().empty().with(new StUnhex()),
            "/org/eolang/maven/pre/classes.xsl",
            "/org/eolang/maven/pre/package.xsl",
            "/org/eolang/maven/pre/tests.xsl",
            "/org/eolang/maven/pre/rename-tests-inners.xsl",
            "/org/eolang/maven/pre/attrs.xsl",
            "/org/eolang/maven/pre/varargs.xsl",
            "/org/eolang/maven/pre/data.xsl"
        ).back(),
        new TrDefault<>(
            new StClasspath(
                "/org/eolang/maven/pre/to-java.xsl",
                String.format("disclaimer %s", new Disclaimer())
            )
        )
    );

    /**
     * The directory where to put pre-transpile files.
     */
    private static final String PRE = "5-pre";

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
        final Collection<ForeignTojo> sources = this.scopedTojos().withSecondXmir();
        final long saved = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(tojo -> () -> this.transpile(tojo), sources)
            )
        ).longValue();
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
     * @param tojo Tojo that should be transpiled.
     * @return Number of transpiled files.
     * @throws IOException If any issues with I/O
     */
    private int transpile(final ForeignTojo tojo) throws IOException {
        final int saved;
        final Path file = tojo.xmirSecond();
        final XML input = new XMLDocument(file);
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path target = place.make(
            this.targetDir.toPath().resolve(TranspileMojo.DIR),
            TranspileMojo.EXT
        );
        final Path src = tojo.source();
        if (
            target.toFile().exists()
                && target.toFile().lastModified() >= file.toFile().lastModified()
                && target.toFile().lastModified() >= src.toFile().lastModified()
        ) {
            Logger.info(
                this, "XMIR %s (%s) were already transpiled to %s",
                new Rel(file), name, new Rel(target)
            );
            saved = 0;
        } else {
            final List<Path> paths = this.transpile(src, input, target);
            paths.forEach(p -> this.transpiledTojos.add(p, file));
            saved = paths.size();
        }
        return saved;
    }

    /**
     * Transpile.
     * @param src The .eo file
     * @param input The .xmir file
     * @param target The path to transpiled .xmir file
     * @return List of Paths to generated java file
     * @throws IOException If any issues with I/O
     */
    private List<Path> transpile(
        final Path src,
        final XML input,
        final Path target
    ) throws IOException {
        final String name = input.xpath("/program/@name").get(0);
        final long removed = this.removeTranspiled(src);
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
            TranspileMojo.TRAIN,
            place.make(this.targetDir.toPath().resolve(TranspileMojo.PRE), "")
        );
        final Path dir = this.targetDir.toPath().resolve(TranspileMojo.DIR);
        new Home(dir).save(new Xsline(trn).pass(input).toString(), dir.relativize(target));
        return new JavaFiles(target, this.generatedDir.toPath()).save();
    }

    /**
     * Remove transpiled files per EO.
     * @param src The eo path
     * @return Count of removed files
     */
    private long removeTranspiled(final Path src) {
        final List<Path> all = this.scopedTojos()
            .withSource(src).stream()
            .map(ForeignTojo::xmirSecond)
            .collect(Collectors.toList());
        long sum = 0;
        for (final Path path : all) {
            sum += this.transpiledTojos.remove(path);
        }
        return sum;
    }
}
