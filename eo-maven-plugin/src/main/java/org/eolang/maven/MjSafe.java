/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.settings.Settings;
import org.cactoos.Scalar;
import org.cactoos.scalar.Unchecked;
import org.cactoos.set.SetOf;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.repository.RemoteRepository;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Abstract Mojo for all others.
 * @since 0.1
 */
abstract class MjSafe extends AbstractMojo {

    /**
     * Maven project.
     */
    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    /**
     * Maven Resolver repository system.
     * Do NOT move this field to a subclass: it is used in both
     * {@link MjResolve} and {@link MjCompile} (indirectly), so it
     * must be injected once here in the base class.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Component
    protected RepositorySystem system;

    /**
     * Maven Resolver repository session, carrying the local repository,
     * mirrors, proxies and credentials from {@code settings.xml}.
     * Do NOT move this field to a subclass: same reason as {@link #system}.
     */
    @Parameter(defaultValue = "${repositorySystemSession}", readonly = true, required = true)
    protected RepositorySystemSession session;

    /**
     * Remote repositories configured for the current project, including
     * any mirror substitutions Maven already applied.
     * Do NOT move this field to a subclass: same reason as {@link #system}.
     */
    @Parameter(defaultValue = "${project.remoteProjectRepositories}", readonly = true)
    protected List<RemoteRepository> repositories;

    /**
     * Directory where classes are stored in target.
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        defaultValue = "${project.build.directory}/classes",
        readonly = true,
        required = true
    )
    protected File classesDir;

    /**
     * File with foreign "tojos".
     */
    @Parameter(
        property = "eo.foreign",
        required = true,
        defaultValue = "${project.build.directory}/eo-foreign.csv"
    )
    protected File foreign;

    /**
     * Format of "foreign" file ("json" or "csv").
     */
    @Parameter(
        alias = "foreignFormat",
        property = "eo.foreignFormat",
        required = true,
        defaultValue = "csv"
    )
    protected String foreignfmt = "csv";

    /**
     * Directory in which .eo files are located.
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        property = "eo.sourcesDir",
        required = true,
        defaultValue = "${project.basedir}/src/main/eo"
    )
    protected File sourcesDir;

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.targetDir",
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    protected File targetDir;

    /**
     * Current scope (either "compile" or "test").
     */
    @Parameter(property = "eo.scope")
    protected String scope = "compile";

    /**
     * The path to a text file where paths of all added
     * .class (and maybe others) files are placed.
     * @since 0.11.0
     */
    @Parameter(
        property = "eo.placed",
        required = true,
        defaultValue = "${project.build.directory}/eo-placed.json"
    )
    protected File placed;

    /**
     * Format of "placed" file ("json" or "csv").
     */
    @Parameter(
        alias = "placedFormat",
        property = "eo.placedFormat",
        required = true,
        defaultValue = "json"
    )
    protected String placedfmt = "json";

    /**
     * Generated sourced directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.generatedDir",
        required = true,
        defaultValue = "${project.build.directory}/generated-sources"
    )
    protected File generatedDir;

    /**
     * The path of the file where XSL measurements (time of execution
     * in milliseconds) will be stored.
     * @since 0.41.0
     */
    @Parameter(
        alias = "xslMeasuresFile",
        property = "eo.xslMeasuresFile",
        required = true,
        defaultValue = "${project.build.directory}/eo/xsl-measures.csv"
    )
    protected File measures;

    /**
     * Mojo execution timeout in seconds.
     *
     * <p>Four hours, which no goal of this plugin has ever needed and a
     * goal that never returns will always exceed, so {@link Deadline} does
     * what its javadoc says it does without a build having to configure
     * anything. The default used to be {@link Integer#MAX_VALUE}, about
     * sixty eight years, which meant the deadline never fired and the
     * thread and the {@link java.util.concurrent.FutureTask} behind it were
     * started for nothing. A build whose goals legitimately take longer
     * raises it through {@code eo.timeout}.</p>
     *
     * @since 0.28.12
     */
    @Parameter(property = "eo.timeout")
    protected Integer timeout = 4 * 60 * 60;

    /**
     * Track optimization steps into intermediate XMIR files?
     * @since 0.24.0
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        alias = "trackTransformationSteps",
        property = "eo.trackTransformationSteps",
        required = true,
        defaultValue = "false"
    )
    protected boolean trackSteps;

    /**
     * If set to TRUE, the exception on exit will be printed in details
     * to the log.
     * @since 0.29.0
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.unrollExitError")
    protected boolean unrollExitError = true;

    /**
     * EO cache directory.
     */
    @Parameter(property = "eo.cache")
    protected File cache = Paths.get(System.getProperty("user.home")).resolve(".eo").toFile();

    /**
     * Use global caching or not.
     * @since 0.55.0
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.cacheEnabled", defaultValue = "true")
    protected boolean cacheEnabled = true;

    /**
     * Rewrite binaries in output directory or not.
     * @since 0.32.0
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.rewriteBinaries", defaultValue = "true")
    protected boolean rewriteBinaries = true;

    /**
     * If we are offline and not able to download anything from the internet.
     * @since 0.32.0
     */
    @Parameter(property = "eo.offline", required = true, defaultValue = "false")
    protected boolean offline;

    /**
     * The Git tag to pull objects from, in objectionary.
     * @since 0.21.0
     */
    @Parameter(property = "eo.tag", required = true, defaultValue = "master")
    protected String tag = "master";

    /**
     * If set to TRUE, experimental lints are skipped during the linting.
     * @since 0.57.0
     * @checkstyle MemberNameCheck (9 lines)
     */
    @Parameter(
        alias = "skipExperimentalLints",
        property = "eo.skipExperimentalLints",
        required = true,
        defaultValue = "false"
    )
    protected boolean skipExperimental;

    /**
     * Pull again even if the .eo file is already present?
     * @since 0.10.0
     */
    @Parameter(
        alias = "overWrite",
        property = "eo.overWrite",
        required = true,
        defaultValue = "false"
    )
    protected boolean overwrite;

    /**
     * Skip artifact with the version 0.0.0.
     * @since 0.9.0
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.skipZeroVersions", required = true, defaultValue = "true")
    protected boolean skipZeroVersions;

    /**
     * Fail resolution process on conflicting dependencies.
     * @since 0.1.0
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        alias = "ignoreVersionConflicts",
        property = "eo.ignoreVersionConflicts",
        required = true,
        defaultValue = "false"
    )
    protected boolean ignoreConflicts;

    /**
     * Shall we discover JAR artifacts for .EO sources?
     * @since 0.12.0
     */
    @Parameter(
        alias = "discoverSelf",
        property = "eo.discoverSelf",
        required = true,
        defaultValue = "false"
    )
    protected boolean discover;

    /**
     * List of inclusion GLOB filters for finding class files while placing them from where
     * they were resolved to classes directory.
     * @since 0.15
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter
    protected Set<String> placeBinaries = new SetOf<>("**");

    /**
     * List of individual lints which must be skipped during the linting.
     * @since 0.57
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter
    protected Set<String> skipSourceLints = new SetOf<>();

    /**
     * List of WPA lints which must be skipped during the linting.
     * @since 0.57
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter
    protected Set<String> skipProgramLints = new SetOf<>();

    /**
     * List of exclusion GLOB filters for finding class files while placing them from where
     * they were resolved to classed directory.
     * @since 0.15
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter
    protected Set<String> skipBinaries = new SetOf<>();

    /**
     * List of inclusion GLOB filters for unplacing and unspiling (ONLY these files will stay).
     * @see <a href="https://news.eolang.org/2022-07-15-placing-and-unplacing.html">Placing and Unplacing in JAR Artifacts</a>
     * @since 0.24
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    protected Set<String> keepBinaries = new SetOf<>();

    /**
     * Add eo-runtime dependency to the classpath.
     *
     * <p>That property is useful only for eo-runtime library compilation.
     * When you compile eo-runtime, you don't want to add eo-runtime from foreign sources
     * (since you compile an eo-runtime library and classpath will anyway have all required classes)
     * and in this case, you should set this property to true.
     * In any other cases, the eo-runtime
     * dependency will be downloaded and added to the classpath automatically.</p>
     *
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.ignoreRuntime", required = true, defaultValue = "false")
    protected boolean ignoreRuntime;

    /**
     * Whether we should fail on warning.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.failOnWarning", required = true, defaultValue = "true")
    protected boolean failOnWarning;

    /**
     * Whether we should lint all the sources together as package.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.lintAsPackage", required = true, defaultValue = "true")
    protected boolean lintAsPackage;

    /**
     * Whether we should skip linting at all.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.skipLinting", required = true, defaultValue = "false")
    protected boolean skipLinting;

    /**
     * The current version of eo-maven-plugin.
     * Maven 3 only.
     * You can read more about that property
     * <a href="https://maven.apache.org/plugin-tools/maven-plugin-tools-annotations/index.html#Supported_Annotations">here</a>.
     */
    @Parameter(defaultValue = "${plugin}", readonly = true)
    protected PluginDescriptor plugin;

    /**
     * Maven settings.
     */
    @Parameter(defaultValue = "${settings}", readonly = true)
    protected Settings settings = new Settings();

    /**
     * The Git hash to pull objects from, computed from {@code tag} field.
     *
     * <p>Built lazily behind {@link ChCached} rather than eagerly from
     * {@code this.tag} here: this field initializer runs during
     * construction, before Maven injects the configured {@code eo.tag}
     * value by reflection, so an eager {@code new ChBrief(this.tag)} would
     * capture the {@code "master"} default forever.</p>
     *
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected CommitHash hash = new ChCached(() -> new ChBrief(this.tag).value());

    /**
     * Resolve default JNA dependency or not.
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    protected boolean jna = true;

    /**
     * Resolve dependencies in central or not.
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    protected boolean centrally = true;

    /**
     * Objectionary.
     * @since 0.50
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Scalar<Objectionary> objectionary = new OyConfigured(
        () -> this.hash,
        () -> this.settings
    );

    /**
     * Whether we should skip goal execution.
     */
    @Parameter(property = "eo.skip", defaultValue = "false")
    private boolean skip;

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }

    @Override
    public final void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        if (this.skip) {
            if (Logger.isInfoEnabled(this)) {
                Logger.info(
                    this, "Execution skipped due to eo.skip option"
                );
            }
        } else {
            final long start = System.nanoTime();
            new Deadline(this, this.timeout, this.unrollExitError).spent(
                () -> {
                    this.exec();
                    return new Object();
                }
            );
            if (Logger.isDebugEnabled(this)) {
                Logger.debug(
                    this,
                    "Execution of %s took %[nano]s",
                    this.getClass().getSimpleName(),
                    System.nanoTime() - start
                );
            }
        }
    }

    /**
     * A fresh foreign catalog in this mojo's scope, to be closed by the
     * caller once the mojo is done with it.
     * @return The catalog
     */
    protected final TjsForeign tojos() {
        return new TjsForeign(
            () -> Catalogs.INSTANCE.make(this.foreign.toPath(), this.foreignfmt),
            () -> this.scope
        );
    }

    /**
     * A fresh placed catalog, to be closed by the caller once the mojo is
     * done with it.
     * @return The catalog
     */
    protected final TjsPlaced placed() {
        return new TjsPlaced(
            () -> Catalogs.INSTANCE.make(this.placed.toPath(), this.placedfmt)
        );
    }

    /**
     * Tojos to use, in "compile" scope only.
     * @return Tojos to use
     */
    protected final TjsForeign compileTojos() {
        return new TjsForeign(
            () -> Catalogs.INSTANCE.make(this.foreign.toPath(), this.foreignfmt),
            () -> "compile"
        );
    }

    /**
     * Exec it.
     * @throws IOException If fails
     */
    abstract void exec() throws IOException;

    Objectionary objectionary() {
        return new Unchecked<>(this.objectionary).value();
    }

    /**
     * Select the Maven EO runtime dependency source.
     * @return Scalar supplying the runtime dependency
     */
    Scalar<Dep> runtime() {
        return new RtChosen(this.project, this.centrally);
    }

    /**
     * Build the assembling step from this mojo's configuration.
     * @param tojos The foreign catalog to assemble through
     * @return Configured Assembling instance
     */
    Assembling assembling(final TjsForeign tojos) {
        return new Assembling(
            tojos,
            new Timed(
                new Parsing(
                    tojos,
                    this.targetDir.toPath(),
                    this.sourcesDir.toPath(),
                    this.caching(Parsing.CACHE)
                )
            ),
            new Timed(
                new Probing(tojos, this.objectionary(), !this.offline)
            ),
            new Timed(
                new Pulling(
                    tojos,
                    this.targetDir.toPath().resolve(Pulling.DIR),
                    this.hash,
                    this.objectionary(),
                    this.cache.toPath().resolve(Pulling.CACHE),
                    this.plugin.getVersion(),
                    this.overwrite,
                    this.cacheEnabled,
                    this.offline
                )
            )
        );
    }

    /**
     * The cache of one step, as configured by the user. This is the only
     * place where {@code eo.cacheEnabled} is read, so that no step below
     * has to know that the option exists.
     * @param sub Directory of that step inside the machine-wide cache
     * @return The cache of that step
     */
    GlobalCache caching(final String sub) {
        return new Caching(this.cache, this.cacheEnabled, this.plugin.getVersion()).forStep(sub);
    }
}
