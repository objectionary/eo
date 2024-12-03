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
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.text.Joined;
import org.eolang.maven.footprint.CachePath;
import org.eolang.maven.footprint.Footprint;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.footprint.FpFork;
import org.eolang.maven.footprint.FpGenerated;
import org.eolang.maven.footprint.FpIfReleased;
import org.eolang.maven.footprint.FpIfTargetExists;
import org.eolang.maven.footprint.FpIgnore;
import org.eolang.maven.footprint.FpUpdateBoth;
import org.eolang.maven.footprint.FpUpdateFromCache;
import org.eolang.maven.optimization.OptSpy;
import org.eolang.maven.optimization.Optimization;
import org.eolang.maven.tojos.AttributeNotFoundException;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.TojoHash;
import org.eolang.maven.util.Threaded;

/**
 * Transpile.
 *
 * @since 0.1
 * @checkstyle ClassFanOutComplexityCheck (400 lines)
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
     * Cache directory for transpiled sources.
     */
    public static final String CACHE = "transpiled";

    /**
     * Java extension.
     */
    public static final String JAVA = "java";

    /**
     * Parsing train with XSLs.
     */
    static final Train<Shift> TRAIN = new TrJoined<>(
        new TrClasspath<>(
            "/org/eolang/maven/pre/classes.xsl",
            "/org/eolang/maven/pre/package.xsl",
            "/org/eolang/maven/pre/tests.xsl",
            "/org/eolang/maven/pre/rename-tests-inners.xsl",
            "/org/eolang/maven/pre/align-test-classes.xsl",
            "/org/eolang/maven/pre/remove-high-level-inner-classes.xsl",
            "/org/eolang/maven/pre/attrs.xsl",
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
        final Collection<ForeignTojo> sources = this.scopedTojos().withShaken();
        final Optimization optimization = this.transpilation();
        final int saved = new Threaded<>(
            sources,
            tojo -> this.transpiled(tojo, optimization)
        ).total();
        Logger.info(
            this, "Transpiled %d XMIRs, created %d Java files in %[file]s",
            sources.size(), saved, this.generatedDir
        );
        if (this.addSourcesRoot) {
            this.project.addCompileSourceRoot(this.generatedDir.getAbsolutePath());
            Logger.info(
                this, "The directory added to Maven 'compile-source-root': %[file]s",
                this.generatedDir
            );
        }
        if (this.addTestSourcesRoot) {
            this.project.addTestCompileSourceRoot(this.generatedDir.getAbsolutePath());
            Logger.info(
                this, "The directory added to Maven 'test-compile-source-root': %[file]s",
                this.generatedDir
            );
        }
    }

    /**
     * Transpile.
     * @param tojo Tojo that should be transpiled.
     * @param transpilation Optimization that transpiles
     * @return Number of transpiled files.
     * @throws java.io.IOException If any issues with I/O
     */
    private int transpiled(final ForeignTojo tojo, final Optimization transpilation)
        throws IOException {
        final Path source;
        try {
            source = tojo.shaken();
        }  catch (final AttributeNotFoundException exception) {
            throw new IllegalStateException(
                "You should check that 'Verify' goal of the plugin was run first",
                exception
            );
        }
        final XML xmir = new XMLDocument(source);
        final String name = xmir.xpath("/program/@name").get(0);
        final Path base = this.targetDir.toPath().resolve(TranspileMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        final Supplier<String> hsh = new TojoHash(tojo);
        final AtomicBoolean rewrite = new AtomicBoolean(false);
        new FpDefault(
            src -> {
                rewrite.set(true);
                return transpilation.apply(xmir).toString();
            },
            this.cache.toPath().resolve(TranspileMojo.CACHE),
            this.plugin.getVersion(),
            hsh,
            base.relativize(target)
        ).apply(source, target);
        return this.javaGenerated(rewrite.get(), target, hsh.get());
    }

    /**
     * Transpile optimization.
     * @return Optimization that transpiles
     */
    private Optimization transpilation() {
        return new OptSpy(
            this.measured(TranspileMojo.TRAIN),
            this.targetDir.toPath().resolve(TranspileMojo.PRE)
        );
    }

    /**
     * Generate java files and count them.
     * @param rewrite Rewrite .java files even if they exist
     * @param target Full target path to XMIR after transpilation optimizations
     * @param hsh Tojo hash
     * @return Amount of generated .java files
     * @throws IOException If fails to save files
     */
    private int javaGenerated(final boolean rewrite, final Path target, final String hsh)
        throws IOException {
        final Collection<XML> nodes = new XMLDocument(target).nodes("//class[java and not(@atom)]");
        final AtomicInteger saved = new AtomicInteger(0);
        for (final XML java : nodes) {
            final Path tgt = new Place(java.xpath("@java-name").get(0)).make(
                this.generatedDir.toPath(), TranspileMojo.JAVA
            );
            final Supplier<Path> che = new CachePath(
                this.cache.toPath().resolve(TranspileMojo.CACHE),
                this.plugin.getVersion(),
                hsh,
                this.generatedDir.toPath().relativize(tgt)
            );
            final Footprint generated = new FpGenerated(
                src -> {
                    saved.incrementAndGet();
                    Logger.debug(
                        this, "Generated %s file from %s",
                        tgt, target
                    );
                    return new Joined(
                        "", java.xpath("java/text()")
                    ).asString();
                }
            );
            new FpIfTargetExists(
                new FpFork(
                    (src, trgt) -> {
                        if (rewrite) {
                            Logger.debug(
                                this,
                                "Rewriting %[file]s because XMIR %[file]s was changed",
                                trgt,
                                target
                            );
                        }
                        return rewrite;
                    },
                    new FpIfReleased(
                        this.plugin.getVersion(),
                        hsh,
                        new FpUpdateBoth(generated, che),
                        generated
                    ),
                    new FpIgnore()
                ),
                new FpIfReleased(
                    this.plugin.getVersion(),
                    hsh,
                    new FpIfTargetExists(
                        trgt -> che.get(),
                        new FpUpdateFromCache(che),
                        new FpUpdateBoth(generated, che)
                    ),
                    generated
                )
            ).apply(Paths.get(""), tgt);
        }
        return saved.get();
    }
}
