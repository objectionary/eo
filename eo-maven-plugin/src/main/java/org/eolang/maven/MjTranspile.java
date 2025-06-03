/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.func.StickyFunc;
import org.cactoos.io.InputOf;
import org.cactoos.text.Joined;
import org.eolang.parser.ObjectName;
import org.eolang.parser.TrFull;

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
public final class MjTranspile extends MjSafe {
    /**
     * The directory where to transpile to.
     */
    static final String DIR = "5-transpile";

    /**
     * The directory where to put pre-transpile files.
     */
    static final String PRE = "5-pre-transpile";

    /**
     * Cache directory for transpiled sources.
     */
    private static final String CACHE = "transpiled";

    /**
     * Java extension.
     */
    private static final String JAVA = "java";

    /**
     * Pattern for replacing EO in package.
     */
    private static final Pattern PACKAGE = Pattern.compile("EO");

    /**
     * Parsing train with XSLs.
     */
    private static final Train<Shift> TRAIN = new TrFull(
        new TrJoined<>(
            new TrClasspath<>(
                "/org/eolang/maven/transpile/set-locators.xsl",
                "/org/eolang/maven/transpile/set-original-names.xsl",
                "/org/eolang/maven/transpile/classes.xsl",
                "/org/eolang/maven/transpile/tests.xsl",
                "/org/eolang/maven/transpile/anonymous-to-nested.xsl",
                "/org/eolang/maven/transpile/package.xsl",
                "/org/eolang/maven/transpile/attrs.xsl",
                "/org/eolang/maven/transpile/data.xsl"
            ).back(),
            new TrDefault<>(
                new StClasspath(
                    "/org/eolang/maven/transpile/to-java.xsl",
                    String.format("disclaimer %s", new Disclaimer())
                )
            )
        )
    );

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
    @SuppressWarnings("PMD.LongVariable")
    private boolean addTestSourcesRoot;

    @Override
    public void exec() throws IOException {
        final Collection<TjForeign> sources = this.scopedTojos().withXmir();
        final Function<XML, XML> transform = this.transpilation();
        final int saved = new Threaded<>(
            sources,
            tojo -> this.transpiled(tojo, transform)
        ).total() + MjTranspile.pinfos(this.generatedDir.toPath());
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
     * @param transform Optimization that transpiles
     * @return Number of transpiled files.
     * @throws java.io.IOException If any issues with I/O
     */
    private int transpiled(
        final TjForeign tojo,
        final Function<XML, XML> transform
    ) throws IOException {
        final Path source = tojo.xmir();
        final XML xmir = new XMLDocument(source);
        final Path base = this.targetDir.toPath().resolve(MjTranspile.DIR);
        final Path target = new Place(new ObjectName(xmir).get()).make(base, MjAssemble.XMIR);
        final Supplier<String> hsh = new TojoHash(tojo);
        final AtomicBoolean rewrite = new AtomicBoolean(false);
        new FpFork(
            (src, tgt) -> new Xnav(xmir.inner())
                .path("/object/o/o[@name='λ']")
                .findAny()
                .isEmpty(),
            new FpDefault(
                src -> {
                    rewrite.compareAndSet(false, true);
                    return transform.apply(xmir).toString();
                },
                this.cache.toPath().resolve(MjTranspile.CACHE),
                this.plugin.getVersion(),
                hsh,
                base.relativize(target),
                this.cacheEnabled
            ),
            new FpIgnore()
        ).apply(source, target);
        return this.javaGenerated(rewrite.get(), target, hsh.get());
    }

    /**
     * Transpile XSL transformations.
     * If {@link MjSafe#trackTransformationSteps} is {@code true} - we create new {@link Xsline}
     * for every XMIR in purpose of thread safety.
     * @return XSL transformations that transpiles XMIR to Java.
     */
    private Function<XML, XML> transpilation() {
        final Train<Shift> measured = this.measured(MjTranspile.TRAIN);
        final Function<XML, XML> func;
        if (this.trackTransformationSteps) {
            func = xml -> new Xsline(
                new TrSpy(
                    measured,
                    new StickyFunc<>(
                        doc -> new Place(new ObjectName(doc).get())
                            .make(this.targetDir.toPath().resolve(MjTranspile.PRE), "")
                    )
                )
            ).pass(xml);
        } else {
            func = new Xsline(measured)::pass;
        }
        return func;
    }

    /**
     * Generate java files and count them.
     * @param rewrite Rewrite .java files even if they exist
     * @param target Full target path to XMIR after transpilation optimizations
     * @param hsh Tojo hash
     * @return Amount of generated .java files
     * @throws IOException If fails to save files
     */
    // java or tests
    // save to target/generated-sources/ or target/generated-test-sources/
    // if clazz has tests -> place them to target/generated-test-sources
    private int javaGenerated(
        final boolean rewrite,
        final Path target,
        final String hsh
    ) throws IOException {
        final AtomicInteger saved = new AtomicInteger(0);
        if (Files.exists(target)) {
            final Collection<Xnav> classes = new Xnav(target)
                .element("object")
                .elements(Filter.withName("class"))
                .collect(Collectors.toList());
            for (final Xnav clazz : classes) {
                final String jname = clazz.attribute("java-name").text().get();
                final Path tgt = new Place(jname).make(
                    this.generatedDir.toPath(), MjTranspile.JAVA
                );
                final Supplier<Path> che = new CachePath(
                    this.cache.toPath().resolve(MjTranspile.CACHE),
                    this.plugin.getVersion(),
                    hsh,
                    this.generatedDir.toPath().relativize(tgt)
                );
                if (clazz.element("tests").text().isPresent()) {
                    final String[] jparts = jname.split("\\.");
                    final Path resolved = Arrays.stream(jparts, 0, jparts.length - 1)
                        .reduce(
                            this.generatedDir.toPath().getParent().resolve(
                                "generated-test-sources"
                            ),
                            Path::resolve,
                            Path::resolve
                        ).resolve(String.format("%sTest.java", jparts[jparts.length - 1]));
                    new Saved(clazz.element("tests").text().get(), resolved).value();
                }
                final Footprint generated = new FpGenerated(
                    src -> {
                        saved.incrementAndGet();
                        Logger.debug(
                            this, "Generated %[file]s (%[size]s) file from %[file]s (%[size]s)",
                            tgt, tgt.toFile().length(), target, target.toFile().length()
                        );
                        return new InputOf(
                            new Joined(
                                "",
                                clazz.elements(Filter.withName("java")).map(
                                    java -> java.text().orElse("")
                                ).collect(Collectors.toList())
                            )
                        );
                    }
                );
                final Footprint both = new FpUpdateBoth(generated, che);
                new FpIfReleased(
                    this.plugin.getVersion(),
                    hsh,
                    new FpFork(
                        (src, trgt) -> {
                            if (rewrite) {
                                Logger.debug(
                                    this,
                                    "Rewriting %[file]s because XMIR %[file]s was changed",
                                    trgt, target
                                );
                            }
                            return rewrite;
                        },
                        new FpFork(this.cacheEnabled, both, generated),
                        new FpIfTargetExists(
                            new FpIgnore(),
                            new FpFork(
                                this.cacheEnabled,
                                new FpIfTargetExists(
                                    trgt -> che.get(),
                                    new FpUpdateFromCache(che),
                                    both
                                ),
                                generated
                            )
                        )
                    ),
                    generated
                ).apply(Paths.get(""), tgt);
            }
        }
        return saved.get();
    }

    /**
     * Create {@code package-info.java} files in all the directories
     * in {@link MjTranspile#generatedDir}.
     * @param generated Path to generated sources
     * @return Amount of created files
     * @throws IOException If fails to create a file
     */
    private static int pinfos(final Path generated) throws IOException {
        final int size;
        if (Files.exists(generated)) {
            final List<Path> dirs = Files.walk(generated)
                .filter(file -> Files.isDirectory(file) && !file.equals(generated))
                .collect(Collectors.toList());
            for (final Path dir : dirs) {
                final String pkg = generated.relativize(dir).toString()
                    .replace(File.separator, ".");
                new Saved(
                    String.join(
                        "\n",
                        "/**",
                        " * This file was auto-generated by eo-maven-plugin,",
                        " * don't modify it, all changes will be lost anyway.",
                        " */",
                        String.format(
                            "// @org.eolang.XmirPackage(\"%s\")",
                            MjTranspile.PACKAGE.matcher(pkg).replaceAll("")
                        ),
                        String.format("package %s;", pkg)
                    ),
                    dir.resolve("package-info.java")
                ).value();
            }
            size = dirs.size();
        } else {
            size = 0;
        }
        return size;
    }
}
