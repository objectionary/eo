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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.optimization.OptCached;
import org.eolang.maven.optimization.OptSpy;
import org.eolang.maven.optimization.OptTrain;
import org.eolang.maven.optimization.Optimization;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Rel;
import org.eolang.parser.ParsingTrain;

/**
 * Optimize XML files.
 *
 * @since 0.1
 */
@Mojo(
    name = "optimize",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class OptimizeMojo extends SafeMojo {

    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "2-optimize";

    /**
     * Subdirectory for optimized cache.
     */
    static final String OPTIMIZED = "optimized";

    /**
     * The directory where to place intermediary files.
     */
    static final String STEPS = "2-optimization-steps";

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
     * Whether we should fail on error.
     *
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
     *
     * @checkstyle MemberNameCheck (10 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(
        property = "eo.failOnWarning",
        required = true,
        defaultValue = "false"
    )
    private boolean failOnWarning;

    @Override
    public void exec() throws IOException {
        final Collection<ForeignTojo> sources = this.scopedTojos().withXmir();
        final Optimization common = this.optimization();
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> this.task(tojo, common),
                    new Filtered<>(
                        ForeignTojo::notOptimized,
                        sources
                    )
                )
            )
        ).intValue();
        if (total > 0) {
            Logger.info(
                this,
                "Optimized %d out of %d XMIR program(s)", total,
                sources.size()
            );
        } else {
            Logger.debug(this, "No XMIR programs out of %d optimized", sources.size());
        }
    }

    /**
     * Converts tojo to optimization task.
     *
     * @param tojo Tojo that should be optimized.
     * @param common Optimization.
     * @return Optimization task.
     */
    private Scalar<Integer> task(
        final ForeignTojo tojo,
        final Optimization common
    ) {
        final Path src = tojo.xmir();
        Logger.debug(
            this, "Adding optimization task for %s",
            src
        );
        return () -> {
            tojo.withOptimized(
                this.make(
                    this.optimization(tojo, common).apply(new XMLDocument(src)),
                    src
                ).toAbsolutePath()
            );
            return 1;
        };
    }

    /**
     * Common optimization for all tojos.
     *
     * @return Optimization for all tojos.
     */
    private Optimization optimization() {
        Optimization opt;
        if (this.trackOptimizationSteps) {
            opt = new OptSpy(this.targetDir.toPath().resolve(OptimizeMojo.STEPS));
        } else {
            opt = new OptTrain();
        }
        if (this.failOnError) {
            opt = new OptTrain(opt, "/org/eolang/parser/fail-on-errors.xsl");
        }
        if (this.failOnWarning) {
            opt = new OptTrain(opt, "/org/eolang/parser/fail-on-warnings.xsl");
        }
        if (this.failOnError) {
            opt = new OptTrain(opt, "/org/eolang/parser/fail-on-critical.xsl");
        } else {
            opt = new OptTrain(opt, new ParsingTrain().empty());
        }
        return opt;
    }

    /**
     * Optimization for specific tojo.
     *
     * @param tojo Tojo
     * @param common Optimization
     * @return Optimization for specific Tojo
     */
    private Optimization optimization(final ForeignTojo tojo, final Optimization common) {
        final Optimization res;
        if (tojo.hasHash()) {
            res = new OptCached(
                common,
                this.cache.resolve(OptimizeMojo.OPTIMIZED).resolve(tojo.hash())
            );
        } else {
            res = common;
        }
        return res;
    }

    /**
     * Make a path with optimized XML file after parsing.
     *
     * @param xml Optimized xml
     * @param file EO file
     * @return The file with optimized XMIR
     * @throws IOException If fails
     */
    private Path make(final XML xml, final Path file) throws IOException {
        final String name = new XMLDocument(file).xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path dir = this.targetDir.toPath();
        final Path target = place.make(
            dir.resolve(OptimizeMojo.DIR), TranspileMojo.EXT
        );
        new Home(dir).save(
            xml.toString(),
            dir.relativize(target)
        );
        Logger.debug(
            this, "Optimized %s (program:%s) to %s",
            new Rel(file), name, new Rel(target)
        );
        return target;
    }
}
