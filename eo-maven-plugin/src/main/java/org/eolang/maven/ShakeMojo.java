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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.nio.file.Path;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.func.StickyFunc;
import org.cactoos.iterable.Filtered;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.TojoHash;
import org.eolang.maven.util.Threaded;

/**
 * Shake (prepare) XMIR for translation to java.
 *
 * @since 0.36
 */
@Mojo(
    name = "shake",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class ShakeMojo extends SafeMojo {

    /**
     * The directory where to shake to.
     */
    public static final String DIR = "2-shake";

    /**
     * Subdirectory for shaken cache.
     */
    static final String CACHE = "shaken";

    /**
     * The directory where to place intermediary files.
     */
    static final String STEPS = "2-shake-steps";

    /**
     * Track optimization steps into intermediate XMIR files?
     *
     * @since 0.24.0
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings("PMD.LongVariable")
    @Parameter(property = "eo.trackTransformationSteps", required = true, defaultValue = "false")
    private boolean trackTransformationSteps;

    @Override
    public void exec() {
        final long start = System.currentTimeMillis();
        final Collection<ForeignTojo> tojos = this.scopedTojos().withXmir();
        final Xsline xsline = this.transformations();
        final int total = new Threaded<>(
            new Filtered<>(
                ForeignTojo::notShaken,
                tojos
            ),
            tojo -> this.shaken(tojo, xsline)
        ).total();
        if (total > 0) {
            Logger.info(
                this,
                "Shaked %d out of %d XMIR program(s) in %[ms]s",
                total, tojos.size(),
                System.currentTimeMillis() - start
            );
        } else {
            Logger.debug(this, "No XMIR programs out of %d shaked", tojos.size());
        }
    }

    /**
     * XMIR shaken to another XMIR.
     * @param tojo Foreign tojo
     * @param xsline Transformations to apply to XMIR
     * @return Amount of optimized XMIR files
     * @throws Exception If fails
     */
    private int shaken(final ForeignTojo tojo, final Xsline xsline)
        throws Exception {
        final Path source = tojo.xmir();
        final XML xmir = new XMLDocument(source);
        final String name = xmir.xpath("/program/@name").get(0);
        final Path base = this.targetDir.toPath().resolve(ShakeMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        tojo.withShaken(
            new FpDefault(
                src -> xsline.pass(xmir).toString(),
                this.cache.toPath().resolve(ShakeMojo.CACHE),
                this.plugin.getVersion(),
                new TojoHash(tojo),
                base.relativize(target)
            ).apply(source, target)
        );
        return 1;
    }

    /**
     * Shake XSL transformations.
     * @return Shake XSL transformations for all tojos.
     */
    private Xsline transformations() {
        final Train<Shift> measured = this.measured(new TrShaking());
        final Train<Shift> train;
        if (this.trackTransformationSteps) {
            train = new TrSpy(
                measured,
                new StickyFunc<>(
                    new ProgramPlace(this.targetDir.toPath().resolve(ShakeMojo.STEPS))
                )
            );
        } else {
            train = measured;
        }
        return new Xsline(train);
    }
}
