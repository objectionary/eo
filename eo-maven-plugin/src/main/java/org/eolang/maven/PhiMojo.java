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
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import net.sf.saxon.expr.instruct.TerminationException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.number.SumOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Rel;
import org.eolang.maven.util.Walk;
import org.eolang.parser.ParsingTrain;
import org.eolang.parser.Schema;

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
        final Home home = new HmBase(this.phiOutputDir);
        final Train<Shift> train = this.train();
        final int count = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    xmir -> () -> {
                        final Path processed = this.phiInputDir.toPath().relativize(xmir);
                        Logger.info(
                            this,
                            "Processing XMIR: %[file]s (%[size]s)",
                            processed, xmir.toFile().length()
                        );
                        final XML xml = new XMLDocument(
                            new TextOf(xmir).asString()
                        );
                        new Schema(xml).check();
                        final Path relative = Paths.get(
                            this.phiInputDir.toPath().relativize(xmir).toString().replace(
                                String.format(".%s", AssembleMojo.XMIR),
                                String.format(".%s", PhiMojo.EXT)
                            )
                        );
                        int amount;
                        try {
                            home.save(PhiMojo.translated(train, xml), relative);
                            Logger.info(
                                this,
                                "Translated to phi: %[file]s (%[size]s) -> %[file]s (%[size]s)",
                                processed,
                                xmir.toFile().length(),
                                relative,
                                this.phiOutputDir.toPath().resolve(relative).toFile().length()
                            );
                            amount = 1;
                        } catch (final ImpossibleToPhiTranslationException exception) {
                            Logger.debug(
                                this,
                                "XML is not translatable to phi:\n%s",
                                xml.toString()
                            );
                            throw new IllegalStateException(
                                String.format("Couldn't translate %s to phi", processed),
                                exception
                            );
                        } catch (final IllegalArgumentException exception) {
                            if (exception.getCause() instanceof TerminationException
                                && this.phiSkipFailed) {
                                Logger.info(
                                    this,
                                    "%[file]s failed on critical error, but skipped because phiSkipFailed=true",
                                    processed
                                );
                                amount = 0;
                            } else {
                                throw exception;
                            }
                        }
                        return amount;
                    },
                    new Walk(this.phiInputDir.toPath())
                )
            )
        ).intValue();
        if (count > 0) {
            Logger.info(
                this, "Translated %d XMIR file(s) from %s to %s",
                count, new Rel(this.phiInputDir), new Rel(this.phiOutputDir)
            );
        } else {
            Logger.info(
                this, "No XMIR files translated from %s",
                new Rel(this.phiInputDir)
            );
        }
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
        return new TrJoined<>(
            train,
            new TrClasspath<>(dependent.toArray(new String[0])).back()
        );
    }

    /**
     * Translate given xmir to phi calculus expression.
     * @param train Train that optimize and translates given xmir
     * @param xmir Text of xmir
     * @return Translated xmir
     * @throws ImpossibleToPhiTranslationException If fails to translate given XMIR to phi
     */
    private static String translated(final Train<Shift> train, final XML xmir)
        throws ImpossibleToPhiTranslationException {
        final XML translated = new Xsline(train).pass(xmir);
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
