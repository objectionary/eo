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
import java.io.File;
import java.nio.file.Path;
import java.util.Set;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;
import org.eolang.maven.objectionary.Objectionaries;
import org.eolang.maven.objectionary.ObjsDefault;

/**
 * Pull all necessary EO XML files from Objectionary and parse them all.
 *
 * @since 0.1
 * @todo #2406:90min Make up with idea how to get rid of duplicate parameters between mojos.
 *  There's a situation where AssembleMojo owns, creates and executes other mojos,
 *  like {@link ParseMojo} or {@link OptimizeMojo}. When we configure our compiler via pom.xml maven
 *  tries to set parameters directly to the calling mojo. That's why we must to have all parameters
 *  from child mojos in AssembleMojo or {@link SafeMojo} (in order they won't be skipped and lost).
 *  That causes duplication of parameters between "parent" mojo and "child" mojos.
 *  Also it obliges the developer to remember that if he adds new parameter to some child mojo,
 *  this parameter must be present in parent mojo as well.
 *  We didn't find a way how we can resolve such duplication at the moment.
 *  So we need to either accept this as impossible to solve or resolve somehow.
 *  Anyway don't forget to remove the puzzle when the decision about the puzzle is made.
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
     * List of inclusion GLOB filters for finding class files.
     * @since 0.15
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle ConstantUsageCheck (5 lines)
     */
    @Parameter
    private final Set<String> includeBinaries = new SetOf<>("**");

    /**
     * List of exclusion GLOB filters for finding class files.
     * @since 0.15
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle ConstantUsageCheck (5 lines)
     */
    @Parameter
    private final Set<String> excludeBinaries = new SetOf<>();

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
     * Track optimization steps into intermediate XML files?
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.24.0
     */
    @SuppressWarnings("PMD.LongVariable")
    @Parameter(property = "eo.trackOptimizationSteps", required = true, defaultValue = "false")
    private boolean trackOptimizationSteps;

    /**
     * The Git tag to pull objects from, in objectionary.
     * @since 0.21.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.tag", required = true, defaultValue = "master")
    private String tag = "master";

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
     * Fail resolution process on conflicting dependencies.
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 1.0
     */
    @Parameter(property = "eo.ignoreVersionConflicts", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.LongVariable")
    private boolean ignoreVersionConflicts;

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

    /**
     * Pull objects from objectionaries or not.
     * @since 0.32.0
     */
    @Parameter(property = "eo.offline", required = true, defaultValue = "false")
    private boolean offline;

    @Override
    public void exec() {
        if (this.central == null) {
            this.central = new Central(this.project, this.session, this.manager);
        }
        String before = this.scopedTojos().status();
        int cycle = 0;
        final Moja<?>[] mojas = {
            new Moja<>(ParseMojo.class),
            new Moja<>(OptimizeMojo.class),
            new Moja<>(ShakeMojo.class),
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
