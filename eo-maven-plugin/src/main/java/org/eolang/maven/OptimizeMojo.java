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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrFast;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.parser.ParsingTrain;
import org.cactoos.Scalar;
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
     * The directory where to place intermediary files.
     */
    public static final String STEPS = "02-steps";

    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "03-optimize";

    /**
     * Subdirectory for parsed cache.
     */
    public static final String OPTIMIZED = "optimized";

    /**
     * EO cache directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.cache")
    @SuppressWarnings("PMD.ImmutableField")
    private Path cache = Paths.get(System.getProperty("user.home")).resolve(".eo");

    /**
     * Parsing train with XSLs.
     */
    private static final Train<Shift> TRAIN = new TrFast(
        new TrClasspath<>(
            new ParsingTrain(),
            "/org/eolang/parser/optimize/globals-to-abstracts.xsl",
            "/org/eolang/parser/optimize/remove-refs.xsl",
            "/org/eolang/parser/optimize/abstracts-float-up.xsl",
            "/org/eolang/parser/optimize/remove-levels.xsl",
            "/org/eolang/parser/add-refs.xsl",
            "/org/eolang/parser/optimize/fix-missed-names.xsl",
            "/org/eolang/parser/add-refs.xsl",
            "/org/eolang/parser/errors/broken-refs.xsl",
            "/org/eolang/parser/optimize/constant-folding.xsl"
        ).back()
    );

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
        final Collection<Tojo> sources = this.scopedTojos().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR)
        );
        int done = 0;
        for (final Tojo tojo : sources) {
            final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR));
            if (tojo.exists(AssembleMojo.ATTR_XMIR2)) {
                final Path tgt = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2));
                if (tgt.toFile().lastModified() >= src.toFile().lastModified()) {
                    Logger.debug(
                        this, "Already optimized %s to %s",
                        new Home().rel(src), new Home().rel(tgt)
                    );
                    continue;
                }
            }
            ++done;
            final XML optimized = this.optimize(src, tojo);
            if (this.shouldPass(optimized)) {
                tojo.set(
                    AssembleMojo.ATTR_XMIR2,
                    this.make(optimized, src).toAbsolutePath().toString()
                );
            }
        }
        if (done > 0) {
            Logger.info(this, "Optimized %d out of %d XMIR program(s)", done, sources.size());
        } else {
            Logger.debug(this, "No XMIR programs out of %d optimized", sources.size());
        }
    }

    /**
     * Optimize XML file after parsing.
     *
     * @param file EO file
     * @return The file with optimized XMIR
     * @throws FileNotFoundException If fails
     */
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.ExceptionAsFlowControl"})
    private XML optimize(final Path file, final Tojo tojo) throws FileNotFoundException {
        final String name = new XMLDocument(file).xpath("/program/@name").get(0);
        Train<Shift> trn = OptimizeMojo.TRAIN;
        final Footprint footprint;
        if(tojo.exists(AssembleMojo.ATTR_HASH)){
            footprint = new FtCached(
                    tojo.get(AssembleMojo.ATTR_HASH),
                    this.targetDir.toPath().resolve(OptimizeMojo.DIR),
                    this.cache.resolve(OptimizeMojo.OPTIMIZED));
        }
        else{
            footprint = new FtDefault(this.targetDir.toPath().resolve(OptimizeMojo.DIR));
        }
        if (this.trackOptimizationSteps) {
            final Place place = new Place(name);
            final Path dir = place.make(
                this.targetDir.toPath().resolve(OptimizeMojo.STEPS), ""
            );
            trn = new SpyTrain(trn, dir);
            Logger.debug(
                this, "Optimization steps will be tracked to %s",
                new Home().rel(dir)
            );
        }
        XML result = new Xsline(trn).pass(new XMLDocument(file));
        try {
            footprint.save(name,
                    AssembleMojo.ATTR_XMIR,
                    result::toString);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return result;
    }

    /**
     * Make path with optimized XML file after parsing.
     *
     * @param xml Optimized xml
     * @param file EO file
     * @return The file with optimized XMIR
     * @throws IOException If fails
     */
    private Path make(final XML xml, final Path file) throws IOException {
        final String name = new XMLDocument(file).xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path target = place.make(
            this.targetDir.toPath().resolve(OptimizeMojo.DIR), TranspileMojo.EXT
        );
        new Home().save(xml.toString(), target);
        Logger.debug(
            this, "Optimized %s (program:%s) to %s",
            new Home().rel(file), name, new Home().rel(target)
        );
        return target;
    }

    /**
     * Should optimization steps pass without errors.
     *
     * @param xml Optimized xml
     * @return Should fail
     */
    private boolean shouldPass(final XML xml) {
        final List<XML> errors = xml.nodes("/program/errors/error");
        return errors.isEmpty() || this.failOnError;
    }

}
