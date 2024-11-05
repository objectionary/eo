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
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import net.sf.saxon.expr.instruct.TerminationException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.number.SumOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.footprint.Saved;
import org.eolang.maven.util.Walk;
import org.eolang.parser.ParsingTrain;
import org.eolang.parser.Schema;
import org.eolang.parser.StMeasured;

/**
 * Read XMIR files and translate them to the phi-calculus expression.
 * @since 0.34.0
 */
@Mojo(
    name = "xmir-to-phi",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class PhiMojo extends SafeMojo {
    /**
     * Extension of the file where we put phi-calculus expression (.phi).
     */
    public static final String EXT = "phi";

    /**
     * The directory where to take xmir files for translation from.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.phiInputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/2-optimize"
    )
    private File phiInputDir;

    /**
     * The directory where to save phi files to.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.phiOutputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/phi"
    )
    private File phiOutputDir;

    /**
     * Whether {@link PhiMojo} should fail on critical errors or not.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.phiFailOnCritical", required = true, defaultValue = "true")
    @SuppressWarnings({"PMD.ImmutableField", "PMD.LongVariable"})
    private boolean phiFailOnCritical = true;

    /**
     * Whether {@link PhiMojo} should fail on errors or not.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.phiFailOnError", required = true, defaultValue = "true")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean phiFailOnError = true;

    /**
     * Whether {@link PhiMojo} should skip XMIRs that failed on critical errors.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.phiSkipFailed", required = true, defaultValue = "false")
    private boolean phiSkipFailed;

    /**
     * Pass XMIR to Optimizations train or not.
     * This flag is used for test in order not to optimize XMIR twice:
     * in {@link OptimizeMojo} and here.
     * @checkstyle MemberNameCheck (5 lines)
     */
    @Parameter(property = "eo.phiOptimize", required = true, defaultValue = "true")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean phiOptimize = true;

    @Override
    public void exec() {
        final AtomicInteger passed = new AtomicInteger();
        final Walk walk = new Walk(this.phiInputDir.toPath());
        final int total = walk.size();
        final Xsline xsline = new Xsline(this.train());
        final int count = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    xmir -> () -> {
                        final int position = passed.addAndGet(1);
                        return this.translate(xmir, xsline, position, total);
                    },
                    walk
                )
            )
        ).intValue();
        if (count > 0) {
            Logger.info(
                this, "Translated %d XMIR file(s) from %[file]s to %[file]s",
                count, this.phiInputDir, this.phiOutputDir
            );
        } else {
            Logger.info(
                this, "No XMIR files translated from %[file]s to %[file]s",
                this.phiInputDir, this.phiOutputDir
            );
        }
    }

    /**
     * Translate one XMIR file to .phi file.
     * @param xmir The XMIR file
     * @param xsline Chain of XSL transformations
     * @param position Its position in the entire pack
     * @param total How many files are there
     * @return How many files translated (either 1 or 0)
     * @throws Exception If fails
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private int translate(final Path xmir, final Xsline xsline, final int position, final int total)
        throws Exception {
        final long start = System.currentTimeMillis();
        Logger.debug(
            this,
            "Processing XMIR (#%d/%d): %[file]s (%[size]s)...",
            position, total, xmir, xmir.toFile().length()
        );
        final XML xml = new XMLDocument(new TextOf(xmir).asString());
        new Schema(xml).check();
        final Path relative = Paths.get(
            this.phiInputDir.toPath().relativize(xmir).toString().replace(
                String.format(".%s", AssembleMojo.XMIR),
                String.format(".%s", PhiMojo.EXT)
            )
        );
        int amount;
        final Path target = this.phiOutputDir.toPath().resolve(relative);
        try {
            new Saved(PhiMojo.translated(xsline, xml), target).value();
            Logger.info(
                this,
                "Translated to phi (#%d/%d): %[file]s (%[size]s) -> %[file]s (%[size]s) in %[ms]s",
                position, total, xmir,
                xmir.toFile().length(),
                relative,
                target.toFile().length(),
                System.currentTimeMillis() - start
            );
            amount = 1;
        } catch (final ImpossibleToPhiTranslationException ex) {
            Logger.debug(this, "XML is not translatable to phi:\n%s", xml.toString());
            throw new IllegalStateException(
                String.format("Couldn't translate %s to phi", xmir), ex
            );
        } catch (final IllegalArgumentException ex) {
            if (ex.getCause() instanceof TerminationException && this.phiSkipFailed) {
                Logger.info(
                    this,
                    "%[file]s failed on critical error, but skipped because phiSkipFailed=true",
                    xmir
                );
                amount = 0;
            } else {
                throw ex;
            }
        }
        return amount;
    }

    /**
     * Build transformations train depends on flags.
     * @return Transformations train
     */
    private Train<Shift> train() {
        final Train<Shift> train;
        if (this.phiOptimize) {
            train = new ParsingTrain();
        } else {
            train = new TrDefault<>();
        }
        final List<String> dependent = new ListOf<>(
            "/org/eolang/parser/critical-errors/duplicate-names.xsl",
            "/org/eolang/maven/phi/incorrect-inners.xsl"
        );
        if (this.phiFailOnError) {
            dependent.add("/org/eolang/parser/fail-on-errors.xsl");
        }
        if (this.phiFailOnCritical) {
            dependent.add("/org/eolang/parser/fail-on-critical.xsl");
        }
        dependent.add("/org/eolang/maven/phi/to-phi.xsl");
        return new TrLambda(
            new TrJoined<>(
                train,
                new TrClasspath<>(dependent.toArray(new String[0])).back()
            ),
            shift -> new StMeasured(
                shift,
                this.targetDir.toPath().resolve("xsl-measures.csv")
            )
        );
    }

    /**
     * Translate given xmir to phi calculus expression.
     * @param xsline Chain of XSL optimizations and transformations
     * @param xmir Text of xmir
     * @return Translated xmir
     * @throws ImpossibleToPhiTranslationException If fails to translate given XMIR to phi
     */
    private static String translated(final Xsline xsline, final XML xmir)
        throws ImpossibleToPhiTranslationException {
        final XML translated = xsline.pass(xmir);
        Logger.debug(PhiMojo.class, "XML after translation to phi:\n%s", translated);
        final List<String> phi = translated.xpath("program/phi/text()");
        if (phi.isEmpty()) {
            throw new ImpossibleToPhiTranslationException(
                "Xpath 'phi/text()' is not found in translated XMIR"
            );
        }
        return phi.get(0);
    }

    /**
     * Exception which indicates that translation to phi can't be processed.
     * @since 0.36.0
     */
    private static class ImpossibleToPhiTranslationException extends Exception {
        /**
         * Ctor.
         * @param cause Cause of the exception.
         */
        ImpossibleToPhiTranslationException(final String cause) {
            super(cause);
        }
    }
}
