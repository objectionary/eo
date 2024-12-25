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
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrFast;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.nio.file.Path;
import java.util.Collection;
import java.util.function.Function;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Filtered;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.TojoHash;
import org.eolang.maven.util.Threaded;
import org.eolang.parser.StEoLogged;
import org.eolang.parser.TrParsing;

/**
 * Optimize XML files, applying a number of mandatory XSL transformations
 * to them.
 *
 * <p>Also, this Mojo runs all available linters and adds errors to the
 * "error" XML element of every XMIR.</p>
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
    static final String CACHE = "optimized";

    /**
     * The directory where to place intermediary files.
     */
    static final String STEPS = "2-optimization-steps";

    /**
     * Track optimization steps into intermediate XML files?
     *
     * @since 0.24.0
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings("PMD.LongVariable")
    @Parameter(property = "eo.trackOptimizationSteps", required = true, defaultValue = "false")
    private boolean trackOptimizationSteps;

    @Override
    public void exec() {
        final long start = System.currentTimeMillis();
        final Collection<ForeignTojo> tojos = this.scopedTojos().withXmir();
        final Function<XML, XML> optimization = this.optimization();
        final int total = new Threaded<>(
            new Filtered<>(
                ForeignTojo::notOptimized,
                tojos
            ),
            tojo -> this.optimized(tojo, optimization)
        ).total();
        if (total > 0) {
            Logger.info(
                this,
                "Optimized %d out of %d XMIR program(s) in %[ms]s",
                total, tojos.size(),
                System.currentTimeMillis() - start
            );
        } else {
            Logger.debug(this, "No XMIR programs out of %d optimized", tojos.size());
        }
    }

    /**
     * XMIR optimized to another XMIR.
     * @param tojo Foreign tojo
     * @param optimization Optimization to apply to XMIR
     * @return Amount of optimized XMIR files
     * @throws Exception If fails
     */
    private int optimized(final ForeignTojo tojo, final Function<XML, XML> optimization)
        throws Exception {
        final Path source = tojo.xmir();
        final XML xmir = new XMLDocument(source);
        final String name = xmir.xpath("/program/@name").get(0);
        final Path base = this.targetDir.toPath().resolve(OptimizeMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        tojo.withOptimized(
            new FpDefault(
                src -> optimization.apply(xmir).toString(),
                this.cache.toPath().resolve(OptimizeMojo.CACHE),
                this.plugin.getVersion(),
                new TojoHash(tojo),
                base.relativize(target)
            ).apply(source, target)
        );
        return 1;
    }

    /**
     * Common optimization for all tojos.
     * @return Optimization for all tojos.
     */
    private Function<XML, XML> optimization() {
        final Function<XML, XML> opt;
        final Train<Shift> train = this.measured(new TrParsing());
        if (this.trackOptimizationSteps) {
            opt = new OptimizeMojo.OptSpy(
                train,
                this.targetDir.toPath().resolve(OptimizeMojo.STEPS)
            );
        } else {
            opt = new OptimizeMojo.OptTrain(train);
        }
        return opt;
    }

    /**
     * Optimization that spies.
     * @since 0.68.0
     */
    static final class OptSpy implements Function<XML, XML> {
        /**
         * Optimizations train.
         */
        private final Train<Shift> train;

        /**
         * Where to track optimization steps.
         */
        private final Path target;

        /**
         * Ctor.
         * @param target Where to track optimization steps.
         */
        OptSpy(final Path target) {
            this(OptTrain.DEFAULT_TRAIN, target);
        }

        /**
         * The main constructor.
         * @param trn Optimizations train.
         * @param target Where to track optimization steps.
         */
        OptSpy(final Train<Shift> trn, final Path target) {
            this.train = trn;
            this.target = target;
        }

        @Override
        public XML apply(final XML xml) {
            final Path dir = new Place(xml.xpath("/program/@name").get(0)).make(this.target, "");
            Logger.debug(this, "Optimization steps will be tracked to %[file]s", dir);
            return new OptTrain(new SpyTrain(this.train, dir)).apply(xml);
        }
    }

    /**
     * Optimisation train of XLS`s.
     * @since 0.68.0
     * @todo #3115:30min Return constant-folding.xsl when it's ready.
     * This optimization was removed from the train because it's not really
     * ready and works only with `bool` object which was removed. We
     * need to make this optimization great again and add to the train.
     */
    static final class OptTrain implements Function<XML, XML> {

        /**
         * Parsing train with XSLs.
         * @implNote The list of applied XSLs is adjusted during execution.
         * <br>Separate instance of the train is used of each optimization
         * thread since {@link com.jcabi.xml.XSLDocument}, which is used under
         * the hood in {@link com.yegor256.xsline.TrClasspath}, is not thread-safe.
         */
        static final Train<Shift> DEFAULT_TRAIN = new TrFast(
            new TrLambda(
                new TrClasspath<>(
                    "/org/eolang/parser/optimize/globals-to-abstracts.xsl",
                    "/org/eolang/parser/optimize/remove-refs.xsl",
                    "/org/eolang/parser/optimize/abstracts-float-up.xsl",
                    "/org/eolang/parser/optimize/remove-levels.xsl",
                    "/org/eolang/parser/add-refs.xsl",
                    "/org/eolang/parser/optimize/fix-missed-names.xsl",
                    "/org/eolang/parser/add-refs.xsl",
                    "/org/eolang/parser/set-locators.xsl",
                    "/org/eolang/parser/blank-xsd-schema.xsl"
                ).back(),
                StEoLogged::new
            ),
            TrFast.class,
            500L
        );

        /**
         * Delegate.
         */
        private final Function<XML, XML> delegate;

        /**
         * Xsline with applied shifts.
         */
        private final Xsline xsline;

        /**
         * The default constructor with the default preset of xsl optimizations.
         */
        OptTrain() {
            this(OptTrain.DEFAULT_TRAIN);
        }

        /**
         * Constructor that accepts train of shifts.
         * @param shifts XLS shifts.
         */
        OptTrain(final Train<Shift> shifts) {
            this(xml -> xml, shifts);
        }

        /**
         * Constructor for single {@link com.yegor256.xsline.StClasspath} optimization.
         * @param delegate Optimizations that have to be done before.
         * @param xls File from classpath.
         */
        OptTrain(final Function<XML, XML> delegate, final String xls) {
            this(
                delegate,
                new TrDefault<Shift>().with(new StClasspath(xls))
            );
        }

        /**
         * Ctor that accepts train of shifts to apply with {@link com.yegor256.xsline.Xsline}.
         * @param delegate Optimizations that have to be done before.
         * @param shifts To apply
         */
        OptTrain(
            final Function<XML, XML> delegate,
            final Train<Shift> shifts
        ) {
            this(
                delegate,
                new Xsline(shifts)
            );
        }

        /**
         * Main ctor.
         * @param delegate Optimizations that have to be done before.
         * @param xsline Xsline with applied shifts.
         */
        OptTrain(
            final Function<XML, XML> delegate,
            final Xsline xsline
        ) {
            this.delegate = delegate;
            this.xsline = xsline;
        }

        @Override
        public XML apply(final XML xml) {
            return this.xsline.pass(
                this.delegate.apply(xml)
            );
        }
    }
}
