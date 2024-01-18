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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.tojos.AttributeNotFoundException;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Rel;

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
     * The directory where to put pre-transpile files.
     */
    public static final String PRE = "7-pre";

    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "8-transpile";

    /**
     * Extension for compiled sources in XMIR format (XML).
     */
    static final String EXT = "xmir";

    /**
     * Parsing train with XSLs.
     */
    static final Train<Shift> TRAIN = new TrJoined<>(
        new TrClasspath<>(
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
     * Java extension.
     */
    private static final Pattern JAVA_EXT = Pattern.compile(".java", Pattern.LITERAL);

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
     * Output.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.outputDir",
        required = true,
        defaultValue = "${project.build.outputDirectory}"
    )
    private File outputDir;

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
    public void exec() {
        final Collection<ForeignTojo> sources = this.scopedTojos().withOptimized();
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
     * @throws java.io.IOException If any issues with I/O
     */
    private int transpile(final ForeignTojo tojo) throws IOException {
        final Path file;
        try {
            file = tojo.verified();
        }  catch (final AttributeNotFoundException exception) {
            throw new IllegalStateException(
                "You should check that 'Verify' goal of the plugin was run first",
                exception
            );
        }
        final XML input = new XMLDocument(file);
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path target = place.make(
            this.targetDir.toPath().resolve(TranspileMojo.DIR),
            TranspileMojo.EXT
        );
        final int saved;
        if (
            target.toFile().exists()
                && target.toFile().lastModified() >= file.toFile().lastModified()
                && target.toFile().lastModified() >= tojo.source().toFile().lastModified()
        ) {
            Logger.info(
                this, "XMIR %s (%s) were already transpiled to %s",
                new Rel(file), name, new Rel(target)
            );
            saved = 0;
        } else {
            final List<Path> paths = this.transpile(input, target);
            paths.forEach(p -> this.transpiledTojos.add(p, file));
            saved = paths.size();
        }
        return saved;
    }

    /**
     * Transpile.
     * @param input The .xmir file
     * @param target The path to transpiled .xmir file
     * @return List of Paths to generated java file
     * @throws java.io.IOException If any issues with I/O
     */
    private List<Path> transpile(
        final XML input,
        final Path target
    ) throws IOException {
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Train<Shift> trn = new SpyTrain(
            TranspileMojo.TRAIN,
            place.make(this.targetDir.toPath().resolve(TranspileMojo.PRE), "")
        );
        final Path dir = this.targetDir.toPath().resolve(TranspileMojo.DIR);
        new HmBase(dir).save(new Xsline(trn).pass(input).toString(), dir.relativize(target));
        final List<Path> javas = new JavaFiles(target, this.generatedDir.toPath()).save();
        this.cleanUpClasses(javas);
        return javas;
    }

    /**
     * Clean up dirty classes.
     * The method is trying to fix problem produced by dirty libraries:
     * <a href="https://github.com/objectionary/eo-strings/issues/147"> eo-strings example </a>
     * Some libraries by mistake can put ALL their compiled classes right into the final library
     * jar, instead of only adding atoms. This can cause different runtime errors since the
     * classpath will contain two classes with the same name:
     * - The first class file will be added from dirty library
     * - The second class with the same name will be compiled from the transpiled java file
     * In order to prevent this, we remove all classes that have the java analog in the
     * generated sources. In other words, if generated-sources (or generated-test-sources) folder
     * has java classes, we expect that they will be only compiled from that folder.
     * _____
     * Synchronization in this method is necessary to prevent
     * {@link java.nio.file.AccessDeniedException} on the Windows OS.
     * You can read more about the original problem in the following issue:
     * - <a href="https://github.com/objectionary/eo/issues/2370">issue link</a>
     * In other words, concurrent file deletions on the Windows OS can lead to an
     * {@link java.nio.file.AccessDeniedException}, which could crash the build.
     * _____
     * @param java The list of java files.
     * @todo #2375:90min. Add concurrency tests for the TranspileMojo.cleanUpClasses method.
     *  We should be sure that the method works correctly in a concurrent environment.
     *  In order to do so we should add a test that will run the cleanUpClasses method in
     *  multiple threads and check that the method works correctly without exceptions.
     *  We can apply the same approach as mentioned in that post:
     *  <a href="https://www.yegor256.com/2018/03/27/how-to-test-thread-safety.html">Post</a>
     */
    private void cleanUpClasses(final Collection<? extends Path> java) {
        final Set<Path> unexpected = java.stream()
            .map(path -> this.generatedDir.toPath().relativize(path))
            .map(TranspileMojo::classExtension)
            .map(binary -> this.outputDir.toPath().resolve(binary))
            .collect(Collectors.toSet());
        for (final Path binary : unexpected) {
            try {
                synchronized (TranspileMojo.class) {
                    Files.deleteIfExists(binary);
                }
            } catch (final IOException cause) {
                throw new IllegalStateException(
                    String.format("Can't delete file %s", binary),
                    cause
                );
            }
        }
    }

    /**
     * Rename java to class.
     * @param java The java file
     * @return The class file with the same (java) content.
     */
    private static Path classExtension(final Path java) {
        final Path result;
        final String filename = TranspileMojo.JAVA_EXT.matcher(java.getFileName().toString())
            .replaceAll(".class");
        if (java.getParent() == null) {
            result = Paths.get(filename);
        } else {
            result = java.getParent().resolve(filename);
        }
        return result;
    }
}
