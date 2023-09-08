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
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.maven.objectionary.Objectionaries;
import org.eolang.maven.objectionary.ObjsDefault;

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
@SuppressWarnings({"PMD.UnusedPrivateField", "PMD.TooManyFields"})
public final class AssembleMojo extends SafeMojo {

    /**
     * The intermediate representation extension.
     */
    public static final String IR_EXTENSION = "xmir";

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
     * Objectionaries.
     * @checkstyle MemberNameCheck (6 lines)
     * @checkstyle ConstantUsageCheck (5 lines)
     */
    private final Objectionaries objectionaries = new ObjsDefault(
        () -> this.cache,
        () -> this.session.getRequest().isUpdateSnapshots()
    );

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

    /**
     * Whether we should fail on warn.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(
        property = "eo.failOnWarning",
        required = true,
        defaultValue = "false"
    )
    private boolean failOnWarning;

    /**
     * Fail resolution process on transitive dependencies.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreTransitive", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean ignoreTransitive;

    /**
     * Add eo-runtime dependency to the classpath.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreRuntime", required = true, defaultValue = "true")
    @SuppressWarnings({"PMD.ImmutableField", "PMD.LongVariable"})
    private boolean withRuntimeDependency = true;

    /**
     * If set to TRUE, the exception on exit will be printed in details
     * to the log.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @since 0.29.0
     */
    @Parameter(property = "eo.unrollExitError")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean unrollExitError = true;

    /**
     * The current version of eo-maven-plugin.
     * Maven 3 only.
     * It is the predefined maven property as  MavenProject, MavenSession, MojoExecution, etc.
     * You can read more about that property
     * <a href="https://maven.apache.org/plugin-tools/maven-plugin-tools-annotations/index.html#Supported_Annotations">here</a>.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(defaultValue = "${plugin}", readonly = true)
    private PluginDescriptor plugin;

    /**
     * Place only binaries that have EO sources inside jar.
     * @since 0.31
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    @SuppressWarnings("PMD.LongVariable")
    private boolean placeBinariesThatHaveSources;

    @Override
    public void exec() throws IOException {
        if (this.central == null) {
            this.central = new Central(this.project, this.session, this.manager);
        }
        String before = this.scopedTojos().status();
        int cycle = 0;
        final Moja<?>[] mojas = {
            new Moja<>(ParseMojo.class),
            new Moja<>(OptimizeMojo.class),
            new Moja<>(DiscoverMojo.class),
            new Moja<>(ProbeMojo.class),
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
            final String after = this.scopedTojos().status();
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
}
