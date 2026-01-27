/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.BiConsumer;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.settings.Settings;
import org.cactoos.scalar.Sticky;
import org.cactoos.set.SetOf;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Abstract Mojo for all others.
 *
 * @since 0.1
 * @checkstyle ClassFanOutComplexityCheck (1000 lines)
 */
@SuppressWarnings({"PMD.TooManyFields", "PMD.TooManyMethods"})
abstract class MjSafe extends AbstractMojo {

    /**
     * Maven project.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    /**
     * Maven session.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(defaultValue = "${session}", readonly = true)
    protected MavenSession session;

    /**
     * Maven plugin manager.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Component
    protected BuildPluginManager manager;

    /**
     * Directory where classes are stored in target.
     * @checkstyle VisibilityModifierCheck (10 lines)
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
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(
        property = "eo.foreign",
        required = true,
        defaultValue = "${project.build.directory}/eo-foreign.json"
    )
    protected File foreign;

    /**
     * Format of "foreign" file ("json" or "csv").
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(property = "eo.foreignFormat", required = true, defaultValue = "csv")
    protected String foreignFormat = "csv";

    /**
     * Directory in which .eo files are located.
     *
     * @checkstyle VisibilityModifierCheck (10 lines)
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
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(
        property = "eo.targetDir",
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    protected File targetDir;

    /**
     * Current scope (either "compile" or "test").
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(property = "eo.scope")
    protected String scope = "compile";

    /**
     * The path to a text file where paths of all added
     * .class (and maybe others) files are placed.
     * @since 0.11.0
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(
        property = "eo.placed",
        required = true,
        defaultValue = "${project.build.directory}/eo-placed.json"
    )
    protected File placed;

    /**
     * Format of "placed" file ("json" or "csv").
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(property = "eo.placedFormat", required = true, defaultValue = "json")
    protected String placedFormat = "json";

    /**
     * Generated sourced directory.
     * @checkstyle VisibilityModifierCheck (10 lines)
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
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(
        property = "eo.xslMeasuresFile",
        required = true,
        defaultValue = "${project.build.directory}/eo/xsl-measures.json"
    )
    protected File xslMeasures;

    /**
     * Mojo execution timeout in seconds.
     * @since 0.28.12
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(property = "eo.timeout")
    protected Integer timeout = Integer.MAX_VALUE;

    /**
     * Track optimization steps into intermediate XMIR files?
     *
     * @since 0.24.0
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @SuppressWarnings("PMD.LongVariable")
    @Parameter(property = "eo.trackTransformationSteps", required = true, defaultValue = "false")
    protected boolean trackTransformationSteps;

    /**
     * If set to TRUE, the exception on exit will be printed in details
     * to the log.
     * @since 0.29.0
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(property = "eo.unrollExitError")
    protected boolean unrollExitError = true;

    /**
     * EO cache directory.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(property = "eo.cache")
    protected File cache = Paths.get(System.getProperty("user.home")).resolve(".eo").toFile();

    /**
     * Use global caching or not.
     * @since 0.55.0
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(property = "eo.cacheEnabled", defaultValue = "true")
    @SuppressWarnings("PMD.ImmutableField")
    protected boolean cacheEnabled = true;

    /**
     * Rewrite binaries in output directory or not.
     * @since 0.32.0
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(property = "eo.rewriteBinaries", defaultValue = "true")
    @SuppressWarnings("PMD.ImmutableField")
    protected boolean rewriteBinaries = true;

    /**
     * If we are offline and not able to download anything from the internet.
     * @since 0.32.0
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(property = "eo.offline", required = true, defaultValue = "false")
    protected boolean offline;

    /**
     * The Git tag to pull objects from, in objectionary.
     * @since 0.21.0
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.tag", required = true, defaultValue = "master")
    protected String tag = "master";

    /**
     * If set to TRUE, experimental lints are skipped during the linting.
     * @since 0.57.0
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.skipExperimentalLints", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.LongVariable")
    protected boolean skipExperimentalLints;

    /**
     * Pull again even if the .eo file is already present?
     * @since 0.10.0
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(property = "eo.overWrite", required = true, defaultValue = "false")
    protected boolean overWrite;

    /**
     * Skip artifact with the version 0.0.0.
     *
     * @since 0.9.0
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(property = "eo.skipZeroVersions", required = true, defaultValue = "true")
    protected boolean skipZeroVersions;

    /**
     * Fail resolution process on conflicting dependencies.
     *
     * @since 0.1.0
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreVersionConflicts", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.LongVariable")
    protected boolean ignoreVersionConflicts;

    /**
     * Shall we discover JAR artifacts for .EO sources?
     *
     * @since 0.12.0
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(property = "eo.discoverSelf", required = true, defaultValue = "false")
    protected boolean discoverSelf;

    /**
     * List of inclusion GLOB filters for finding class files while placing them from where
     * they were resolved to classes directory.
     * @since 0.15
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter
    protected Set<String> placeBinaries = new SetOf<>("**");

    /**
     * List of individual lints which must be skipped during the linting.
     * @since 0.57
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter
    protected Set<String> skipSourceLints = new SetOf<>();

    /**
     * List of WPA lints which must be skipped during the linting.
     * @since 0.57
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter
    protected Set<String> skipProgramLints = new SetOf<>();

    /**
     * List of exclusion GLOB filters for finding class files while placing them from where
     * they were resolved to classed directory.
     * @since 0.15
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter
    protected Set<String> skipBinaries = new SetOf<>();

    /**
     * List of inclusion GLOB filters for unplacing and unspiling (ONLY these files will stay).
     * @see <a href="https://news.eolang.org/2022-07-15-placing-and-unplacing.html">Placing and Unplacing in JAR Artifacts</a>
     * @since 0.24
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
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
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreRuntime", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.ImmutableField")
    protected boolean ignoreRuntime;

    /**
     * Fail resolution process on transitive dependencies.
     *
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreTransitive", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.ImmutableField")
    protected boolean ignoreTransitive;

    /**
     * Whether we should fail on warning.
     *
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.failOnWarning", required = true, defaultValue = "true")
    protected boolean failOnWarning;

    /**
     * Whether we should lint all the sources together as package.
     *
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.lintAsPackage", required = true, defaultValue = "true")
    protected boolean lintAsPackage;

    /**
     * Whether we should skip linting at all.
     *
     * @checkstyle MemberNameCheck (10 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.skipLinting", required = true, defaultValue = "false")
    protected boolean skipLinting;

    /**
     * The current version of eo-maven-plugin.
     * Maven 3 only.
     * You can read more about that property
     * <a href="https://maven.apache.org/plugin-tools/maven-plugin-tools-annotations/index.html#Supported_Annotations">here</a>.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (7 lines)
     */
    @Parameter(defaultValue = "${plugin}", readonly = true)
    protected PluginDescriptor plugin;

    /**
     * Maven settings.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(defaultValue = "${settings}", readonly = true)
    protected Settings settings;

    /**
     * Placed tojos.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected final TjsPlaced placedTojos = new TjsPlaced(
        new Sticky<>(() -> Catalogs.INSTANCE.make(this.placed.toPath(), this.placedFormat))
    );

    /**
     * The central.
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    protected BiConsumer<Dependency, Path> central;

    /**
     * The Git hash to pull objects from.
     * If not set, will be computed from {@code tag} field.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected CommitHash hash = new ChCached(
        new ChNarrow(new ChRemote(this.tag))
    );

    /**
     * Cached tojos.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    private final TjsForeign tojos = new TjsForeign(
        () -> Catalogs.INSTANCE.make(this.foreign.toPath(), this.foreignFormat),
        () -> this.scope
    );

    /**
     * Whether we should skip goal execution.
     */
    @Parameter(property = "eo.skip", defaultValue = "false")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean skip;

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }

    /**
     * Execute it.
     * @throws MojoFailureException If fails during build
     * @checkstyle NoJavadocForOverriddenMethodsCheck (10 lines)
     * @checkstyle CyclomaticComplexityCheck (70 lines)
     */
    @Override
    @SuppressWarnings("PMD.CognitiveComplexity")
    public final void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        if (this.skip) {
            if (Logger.isInfoEnabled(this)) {
                Logger.info(
                    this, "Execution skipped due to eo.skip option"
                );
            }
        } else {
            try {
                if (this.central == null) {
                    this.central = new Central(this.project, this.session, this.manager);
                }
                final long start = System.nanoTime();
                this.execWithTimeout();
                if (Logger.isDebugEnabled(this)) {
                    Logger.debug(
                        this,
                        "Execution of %s took %[nano]s",
                        this.getClass().getSimpleName(),
                        System.nanoTime() - start
                    );
                }
            } catch (final TimeoutException ex) {
                this.exitError(
                    Logger.format(
                        "Timeout %[ms]s for Mojo execution is reached",
                        TimeUnit.SECONDS.toMillis(this.timeout)
                    ),
                    ex
                );
            } catch (final ExecutionException ex) {
                this.exitError(
                    String.format("'%s' execution failed", this),
                    ex
                );
            } finally {
                if (this.foreign != null) {
                    MjSafe.closeTojos(this.tojos);
                }
                if (this.placed != null) {
                    MjSafe.closeTojos(this.placedTojos);
                }
            }
        }
    }

    /**
     * Tojos to use, in my scope only.
     * @return Tojos to use
     * @checkstyle AnonInnerLengthCheck (100 lines)
     */
    protected final TjsForeign scopedTojos() {
        return this.tojos;
    }

    /**
     * Tojos to use, in "compile" scope only.
     * @return Tojos to use
     * @checkstyle AnonInnerLengthCheck (100 lines)
     */
    protected final TjsForeign compileTojos() {
        return new TjsForeign(
            () -> Catalogs.INSTANCE.make(this.foreign.toPath(), this.foreignFormat),
            () -> "compile"
        );
    }

    /**
     * Make a measured train from another train.
     * @param train The train
     * @return Measured train
     */
    protected final Train<Shift> measured(final Train<Shift> train) {
        if (this.xslMeasures.getParentFile().mkdirs()) {
            Logger.debug(this, "Directory created for %[file]s", this.xslMeasures);
        }
        if (!this.xslMeasures.getParentFile().exists()) {
            throw new IllegalArgumentException(
                String.format(
                    "For some reason, the directory %s is absent, can't write measures to %s",
                    this.xslMeasures.getParentFile(),
                    this.xslMeasures
                )
            );
        }
        if (this.xslMeasures.isDirectory()) {
            throw new IllegalArgumentException(
                String.format(
                    "This is not a file but a directory, can't write to it: %s",
                    this.xslMeasures
                )
            );
        }
        return new TrLambda(
            train,
            shift -> new StMeasured(
                shift,
                this.xslMeasures.toPath()
            )
        );
    }

    /**
     * Exec it.
     * @throws IOException If fails
     */
    abstract void exec() throws IOException;

    /**
     * Get active proxy from Maven settings.
     * @return Proxy if any.
     */
    Proxy[] proxies() {
        return Optional.ofNullable(this.settings)
            .map(Settings::getProxies)
            .orElse(List.of())
            .stream()
            .filter(org.apache.maven.settings.Proxy::isActive)
            .map(
                p -> new Proxy(
                    Proxy.Type.HTTP,
                    new InetSocketAddress(p.getHost(), p.getPort())
                )
            ).toArray(Proxy[]::new);
    }

    /**
     * Runs exec command with timeout if needed.
     *
     * @throws ExecutionException If unexpected exception happened during execution
     * @throws TimeoutException If timeout limit reached
     */
    @SuppressWarnings("PMD.CloseResource")
    private void execWithTimeout() throws ExecutionException, TimeoutException {
        final ExecutorService service = Executors.newSingleThreadExecutor();
        try {
            service.submit(
                () -> {
                    this.exec();
                    return new Object();
                }
            ).get(this.timeout.longValue(), TimeUnit.SECONDS);
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                Logger.format(
                    "Timeout %[ms]s thread was interrupted",
                    TimeUnit.SECONDS.toMillis(this.timeout.longValue())
                ),
                ex
            );
        } finally {
            boolean terminated = false;
            service.shutdown();
            while (!terminated) {
                try {
                    terminated = service.awaitTermination(60, TimeUnit.SECONDS);
                    if (terminated) {
                        service.shutdownNow();
                    }
                } catch (final InterruptedException ex) {
                    service.shutdownNow();
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

    /**
     * Close it safely.
     * @param res The resource
     * @throws MojoFailureException If fails
     */
    private static void closeTojos(final Closeable res) throws MojoFailureException {
        try {
            res.close();
        } catch (final IOException ex) {
            throw new MojoFailureException(ex);
        }
    }

    /**
     * Make an error for the exit and throw it.
     * @param msg The message
     * @param exp Original problem
     * @throws MojoFailureException For sure
     */
    private void exitError(final String msg, final Throwable exp)
        throws MojoFailureException {
        if (!this.unrollExitError) {
            return;
        }
        final MojoFailureException out = new MojoFailureException(msg, exp);
        final List<String> causes = MjSafe.causes(exp);
        for (int pos = 0; pos < causes.size(); ++pos) {
            final String cause = causes.get(pos);
            if (cause == null) {
                causes.remove(pos);
                break;
            }
        }
        int idx = 0;
        while (true) {
            if (idx >= causes.size()) {
                break;
            }
            final String cause = causes.get(idx);
            for (int later = idx + 1; later < causes.size(); ++later) {
                final String another = causes.get(later);
                if (another != null && cause.contains(another)) {
                    causes.remove(idx);
                    idx -= 1;
                    break;
                }
            }
            idx += 1;
        }
        for (final String cause : new LinkedHashSet<>(causes)) {
            Logger.error(this, cause);
        }
        throw out;
    }

    /**
     * Turn the exception into an array of causes.
     * @param exp Original problem
     * @return List of causes
     */
    private static List<String> causes(final Throwable exp) {
        final List<String> causes = new LinkedList<>();
        causes.add(exp.getMessage());
        final Throwable cause = exp.getCause();
        if (cause != null) {
            causes.addAll(MjSafe.causes(cause));
        }
        return causes;
    }
}
