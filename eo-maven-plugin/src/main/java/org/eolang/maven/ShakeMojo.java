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
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Rel;

@Mojo(
    name = "shake",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class ShakeMojo extends SafeMojo {
    /**
     * The directory where to shake to.
     */
    public static final String DIR = "3-shake";

    /**
     * Subdirectory for shaken cache.
     */
    static final String SHAKEN = "shaken";

    /**
     * The directory where to place intermediary files.
     */
    static final String STEPS = "3-shake-steps";

    /**
     * Track optimization steps into intermediate XML files?
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.24.0
     */
    @SuppressWarnings("PMD.LongVariable")
    @Parameter(property = "eo.trackOptimizationSteps", required = true, defaultValue = "false")
    private boolean trackOptimizationSteps;

    @Override
    void exec() throws IOException {
        final Collection<ForeignTojo> tojos = this.scopedTojos().withOptimized();
        final Optimization opt = this.optimization();
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> this.task(tojo, opt),
                    new Filtered<>(
                        ForeignTojo::notShaken,
                        tojos
                    )
                )
            )
        ).intValue();
        if (total > 0) {
            Logger.info(
                this,
                "Shaken %d out of %d XMIR program(s)", total,
                tojos.size()
            );
        } else {
            Logger.debug(this, "No XMIR programs out of %d shaken", tojos.size());
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
        final Path src = tojo.optimized();
        Logger.debug(
            this, "Adding optimization task for %s",
            src
        );
        return () -> {
            tojo.withShaken(
                this.make(
                    this.optimization(tojo, common).apply(new XMLDocument(src)),
                    src
                ).toAbsolutePath()
            );
            return 1;
        };
    }

    /**
     * Shake optimizations for tojos.
     * @return Shake optimizations
     */
    private Optimization optimization() {
        final Optimization opt;
        if (this.trackOptimizationSteps) {
            opt = new OptSpy(this.targetDir.toPath().resolve(ShakeMojo.STEPS));
        } else {
            opt = new OptTrain();
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
                this.cache.resolve(ShakeMojo.SHAKEN).resolve(tojo.hash())
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
            dir.resolve(ShakeMojo.DIR), TranspileMojo.EXT
        );
        new HmBase(dir).save(
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
