/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Predicate;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
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
        defaultValue = "${project.build.directory}/eo/foreign.csv"
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
        defaultValue = "${project.build.directory}/eo/placed.csv"
    )
    protected File placed;

    /**
     * Format of "placed" file ("json" or "csv").
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(property = "eo.placedFormat", required = true, defaultValue = "csv")
    protected String placedFormat = "csv";

    /**
     * The path to a text file where paths of generated java files per EO program.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (10 lines)
     * @since 0.11.0
     */
    @Parameter(
        property = "eo.transpiled",
        required = true,
        defaultValue = "${project.build.directory}/eo/transpiled.csv"
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
     * Cached tojos.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected final Unchecked<Tojos> tojos = new Unchecked<>(
        new Sticky<>(
            () -> Catalogs.INSTANCE.make(this.foreign.toPath(), this.foreignFormat)
        )
    );

    /**
     * Cached placed tojos.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected final Unchecked<Tojos> placedTojos = new Unchecked<>(
        new Sticky<>(
            () -> Catalogs.INSTANCE.make(this.placed.toPath(), this.placedFormat)
        )
    );

    /**
     * Cached transpiled tojos.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    protected final Unchecked<Tojos> transpiledTojos = new Unchecked<>(
        new Sticky<>(
            () -> Catalogs.INSTANCE.make(this.transpiled.toPath(), this.transpiledFormat)
        )
    );

    /**
     * Whether we should skip goals execution.
     */
    @Parameter(property = "eo.skip", defaultValue = "false")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean skip;

    @Override
    public final void execute() throws MojoFailureException, MojoExecutionException {
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
            } catch (final IOException ex) {
                throw new MojoFailureException(
                    String.format(
                        "Failed to execute %s",
                        this.getClass().getCanonicalName()
                    ),
                    ex
                );
            } catch (final TimeoutException ex) {
                throw new MojoExecutionException(
                    Logger.format(
                        "Timeout %[ms]s for Mojo execution is reached",
                        TimeUnit.SECONDS.toMillis(this.timeout)
                    ),
                    ex
                );
            } catch (final ExecutionException ex) {
                throw new MojoExecutionException(
                    String.format("'%s' execution failed", this),
                    ex
                );
            } finally {
                if (this.foreign != null) {
                    SafeMojo.closeTojos(this.tojos.value());
                }
                if (this.placed != null) {
                    SafeMojo.closeTojos(this.placedTojos.value());
                }
                if (this.transpiled != null) {
                    SafeMojo.closeTojos(this.transpiledTojos.value());
                }
            }
        }
    }

    /**
     * Tojos to use, in my scope only.
     * @return Tojos to use
     * @checkstyle AnonInnerLengthCheck (100 lines)
     */
    protected final Tojos scopedTojos() {
        final Tojos unscoped = this.tojos.value();
        return new Tojos() {
            @Override
            public void close() throws IOException {
                unscoped.close();
            }

            @Override
            public Tojo add(final String name) {
                final Tojo tojo = unscoped.add(name);
                if (!tojo.exists(AssembleMojo.ATTR_SCOPE)) {
                    tojo.set(AssembleMojo.ATTR_SCOPE, SafeMojo.this.scope);
                }
                return tojo;
            }

            @Override
            public List<Tojo> select(final Predicate<Tojo> filter) {
                return unscoped.select(
                    t -> filter.test(t)
                        && (
                        t.get(AssembleMojo.ATTR_SCOPE).equals(SafeMojo.this.scope)
                            || "test".equals(SafeMojo.this.scope)
                    )
                );
            }
        };
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
     * @throws IOException If fails
     */
    private void execWithTimeout() throws ExecutionException, TimeoutException, IOException {
        try {
            Executors.newSingleThreadExecutor().submit(
                () -> {
                    this.exec();
                    return new Object();
                }
            ).get(this.timeout, TimeUnit.SECONDS);
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                Logger.format(
                    "Timeout %[ms]s thread was interrupted",
                    TimeUnit.SECONDS.toMillis(this.timeout)
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
}
