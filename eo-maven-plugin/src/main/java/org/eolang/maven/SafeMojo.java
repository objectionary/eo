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
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.cactoos.scalar.Sticky;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.tojos.ForeignTojos;
import org.eolang.maven.tojos.PlacedTojos;
import org.eolang.maven.tojos.TranspiledTojos;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Abstract Mojo for all others.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyFields")
abstract class SafeMojo extends AbstractMojo {

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
     * File with foreign "tojos".
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(
        property = "eo.foreign",
        required = true,
        defaultValue = "${project.build.directory}/eo-foreign.csv"
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
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @since 0.11.0
     */
    @Parameter(
        property = "eo.placed",
        required = true,
        defaultValue = "${project.build.directory}/eo-placed.csv"
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
     * The path to a text file where paths of generated java files per EO program.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @since 0.11.0
     */
    @Parameter(
        property = "eo.transpiled",
        required = true,
        defaultValue = "${project.build.directory}/eo-transpiled.csv"
    )
    protected File transpiled;

    /**
     * Mojo execution timeout in seconds.
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @since 0.28.12
     */
    @Parameter(property = "eo.timeout")
    protected Integer timeout = Integer.MAX_VALUE;

    /**
     * Format of "transpiled" file ("json" or "csv").
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(property = "eo.transpiledFormat", required = true, defaultValue = "csv")
    protected String transpiledFormat = "csv";

    /**
     * If set to TRUE, the exception on exit will be printed in details
     * to the log.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @since 0.29.0
     */
    @Parameter(property = "eo.unrollExitError")
    protected boolean unrollExitError = true;

    /**
     * EO cache directory.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     */
    @Parameter(property = "eo.cache")
    protected Path cache = Paths.get(System.getProperty("user.home")).resolve(".eo");

    /**
     * Used for object versioning implementation.
     * If set to TRUE - objects are parsed, stored in tojos and processed as versioned.
     * @todo #1602:30min Remove the flag when objection versioned is
     *  implemented. The variable is used for implementation of object
     *  versioning. When object versioning is implemented there
     *  will be no need for that variable
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.withVersions", defaultValue = "false")
    protected boolean withVersions;

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
     * Commit hashes.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected final Map<String, ? extends CommitHash> hashes = new CommitHashesMap();

    /**
     * Placed tojos.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected final PlacedTojos placedTojos = new PlacedTojos(
        new Sticky<>(() -> Catalogs.INSTANCE.make(this.placed.toPath(), this.placedFormat))
    );

    /**
     * Cached transpiled tojos.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected final TranspiledTojos transpiledTojos = new TranspiledTojos(
        new Sticky<>(() -> Catalogs.INSTANCE.make(this.transpiled.toPath(), this.transpiledFormat))
    );

    /**
     * Cached tojos.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    private final ForeignTojos tojos = new ForeignTojos(
        () -> Catalogs.INSTANCE.make(this.foreign.toPath(), this.foreignFormat),
        () -> this.scope
    );

    /**
     * Whether we should skip goal execution.
     */
    @Parameter(property = "eo.skip", defaultValue = "false")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean skip;

    /**
     * Execute it.
     * @throws MojoFailureException If fails during build
     * @checkstyle NoJavadocForOverriddenMethodsCheck (10 lines)
     * @checkstyle CyclomaticComplexityCheck (70 lines)
     */
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
            try {
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
                    SafeMojo.closeTojos(this.tojos);
                }
                if (this.placed != null) {
                    SafeMojo.closeTojos(this.placedTojos);
                }
                if (this.transpiled != null) {
                    SafeMojo.closeTojos(this.transpiledTojos);
                }
            }
        }
    }

    /**
     * Tojos to use, in my scope only.
     * @return Tojos to use
     * @checkstyle AnonInnerLengthCheck (100 lines)
     */
    protected final ForeignTojos scopedTojos() {
        return this.tojos;
    }

    /**
     * Exec it.
     * @throws IOException If fails
     */
    abstract void exec() throws IOException;

    /**
     * Runs exec command with timeout if needed.
     *
     * @throws ExecutionException If unexpected exception happened during execution
     * @throws TimeoutException If timeout limit reached
     */
    private void execWithTimeout() throws ExecutionException, TimeoutException {
        try {
            Executors.newSingleThreadExecutor().submit(
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
        final MojoFailureException out = new MojoFailureException(msg, exp);
        if (this.unrollExitError) {
            final List<String> causes = SafeMojo.causes(exp);
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
            causes.addAll(SafeMojo.causes(cause));
        }
        return causes;
    }
}
