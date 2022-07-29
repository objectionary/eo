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
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Pull all necessary EO XML files from Objectionary and parse them all.
 *
 * @since 0.1
 */
@Mojo(
    name = "assemble",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
@SuppressWarnings("PMD.UnusedPrivateField")
public final class AssembleMojo extends SafeMojo {

    /**
     * Tojo ATTR.
     */
    public static final String ATTR_EO = "eo";

    /**
     * Tojo ATTR.
     */
    public static final String ATTR_VERSION = "version";

    /**
     * Tojo ATTR.
     */
    public static final String ATTR_XMIR = "xmir";

    /**
     * Tojo ATTR.
     */
    public static final String ATTR_XMIR2 = "xmir2";

    /**
     * Tojo ATTR.
     */
    public static final String ATTR_JAR = "jar";

    /**
     * Tojo ATTR.
     */
    public static final String ATTR_DISCOVERED = "discovered";

    /**
     * Where this object was discovered.
     */
    @SuppressWarnings("PMD.LongVariable")
    public static final String ATTR_DISCOVERED_AT = "discovered-at";

    /**
     * Tojo ATTR.
     */
    public static final String ATTR_SCOPE = "scope";

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
     * The objectionary.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Objectionary objectionary;

    /**
     * The central.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private BiConsumer<Dependency, Path> central;

    /**
     * Pull again even if the .eo file is already present?
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.10.0
     */
    @Parameter(property = "eo.overWrite", required = true, defaultValue = "false")
    private boolean overWrite;

    /**
     * The Git hash to pull objects from, in objectionary.
     * @since 0.21.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.hash", required = true, defaultValue = "master")
    private String hash = "master";

    /**
     * The path to a text file where paths of all added
     * .class (and maybe others) files are placed.
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.11.0
     */
    @Parameter(
        property = "eo.placed",
        required = true,
        defaultValue = "${project.build.directory}/eo/placed.csv"
    )
    private File placed;

    /**
     * Format of "placed" file ("json" or "csv").
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.placedFormat", required = true, defaultValue = "csv")
    private String placedFormat = "csv";

    /**
     * Skip artifact with the version 0.0.0.
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.9.0
     */
    @Parameter(property = "eo.skipZeroVersions", required = true, defaultValue = "true")
    private boolean skipZeroVersions;

    /**
     * Shall we discover JAR artifacts for .EO sources?
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.12.0
     */
    @Parameter(property = "eo.discoverSelf", required = true, defaultValue = "false")
    private boolean discoverSelf;

    /**
     * Track optimization steps into intermediate XML files?
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.24.0
     */
    @SuppressWarnings("PMD.LongVariable")
    @Parameter(property = "eo.trackOptimizationSteps", required = true, defaultValue = "false")
    private boolean trackOptimizationSteps;

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

    @Override
    public void exec() throws IOException {
        if (this.central == null) {
            this.central = new Central(this.project, this.session, this.manager);
        }
        String before = this.status();
        int cycle = 0;
        final Moja<?>[] mojas = {
            new Moja<>(ParseMojo.class),
            new Moja<>(OptimizeMojo.class),
            new Moja<>(DiscoverMojo.class),
            new Moja<>(PullMojo.class),
            new Moja<>(ResolveMojo.class),
            new Moja<>(MarkMojo.class),
            new Moja<>(PlaceMojo.class),
        };
        while (true) {
            final long start = System.nanoTime();
            for (final Moja<?> moja : mojas) {
                moja.copy(this).execute();
            }
            final String after = this.status();
            ++cycle;
            if (Logger.isInfoEnabled(this)) {
                Logger.info(
                    this, "Assemble cycle #%d (%s -> %s), took %[nano]s",
                    cycle, before, after, System.nanoTime() - start
                );
            }
            if (after.equals(before)) {
                break;
            }
            before = after;
        }
        Logger.info(
            this, "%d assemble cycle(s) produced some new object(s): %s",
            cycle, before
        );
    }

    /**
     * Status of tojos.
     * @return Status in text
     */
    private String status() {
        final String[] attrs = {
            AssembleMojo.ATTR_EO,
            AssembleMojo.ATTR_XMIR,
            AssembleMojo.ATTR_XMIR2,
            AssembleMojo.ATTR_DISCOVERED,
        };
        final Collection<String> parts = new LinkedList<>();
        for (final String attr : attrs) {
            parts.add(
                String.format(
                    "%s:%d",
                    attr,
                    this.scopedTojos().select(tojo -> tojo.exists(attr)).size()
                )
            );
        }
        return String.join("/", parts);
    }

}
