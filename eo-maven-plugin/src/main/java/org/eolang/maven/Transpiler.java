/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
import com.yegor256.xsline.TrBulk;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.cactoos.text.Joined;
import org.eolang.parser.ErrorSeverity;
import org.eolang.parser.ErrorSeverity.EsBase;
import org.eolang.parser.ParsingTrain;

/**
 * An abstract transpiler.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
final class Transpiler {

    /**
     * Extension for compiled sources in XMIR format (XML).
     */
    public static final String EXT = "xmir";

    /**
     * Temp dir.
     */
    private final Path temp;

    /**
     * Dir with pre-files.
     */
    private final Path pre;

    /**
     * For what severities transpiling must fail.
     */
    private final Set<ErrorSeverity> sevtofail;

    /**
     * Ctor.
     * @param tmp The temp
     * @param ppre The pre
     * @param sevtofail For what sevtofail to fail
     */
    Transpiler(final Path tmp, final Path ppre, final Set<ErrorSeverity> sevtofail) {
        this.temp = tmp;
        this.pre = ppre;
        this.sevtofail = sevtofail;
    }

    /**
     * Ctor.
     * @param tmp The temp
     * @param ppre The pre
     */
    Transpiler(final Path tmp, final Path ppre) {
        this(
            tmp,
            ppre,
            new HashSet<>(Collections.singletonList(ErrorSeverity.ERROR))
        );
    }

    /**
     * Transpile.
     *
     * @param file The path to the .xmir file
     * @param generated The path to the directory, where .java files to be saved
     * @return How many .java files generated
     * @throws IOException If any issues with I/O
     */
    public int transpile(final Path file, final Path generated) throws IOException {
        final XML input = new XMLDocument(file);
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path target = place.make(this.temp, Transpiler.EXT);
        int total = 0;
        if (
            target.toFile().exists()
                && target.toFile().lastModified() >= file.toFile().lastModified()
        ) {
            Logger.info(
                this, "XMIR %s (%s) already transpiled to %s",
                Save.rel(file), name, Save.rel(target)
            );
        } else {
            Train<Shift> train = new TrBulk<>(new TrClasspath<>(new ParsingTrain().empty())).with(
                Arrays.asList(
                    "/org/eolang/maven/pre/classes.xsl",
                    "/org/eolang/maven/pre/junit.xsl",
                    "/org/eolang/maven/pre/rename-junit-inners.xsl",
                    "/org/eolang/maven/pre/attrs.xsl",
                    "/org/eolang/maven/pre/varargs.xsl",
                    "/org/eolang/maven/pre/data.xsl",
                    "/org/eolang/maven/pre/to-java.xsl"
                )
            ).back().back();
            train = new SpyTrain(train, place.make(this.pre, ""));
            final XML out = new Xsline(train).pass(input);
            new Save(out.toString(), target).saveQuietly();
            final XML after =
                new EnsureNoErrors(
                    out,
                    name,
                    this.sevtofail
                ).validate();
            final Collection<XML> nodes = after.nodes("//class[java and not(@atom)]");
            if (nodes.isEmpty()) {
                Logger.info(
                    this, "Transpiled %s but no .java files created",
                    Save.rel(file)
                );
            } else {
                for (final XML java : nodes) {
                    Transpiler.saveJava(java, generated);
                    ++total;
                }
                Logger.info(
                    this, "Transpiled %s to %s, created %d .java file(s)",
                    Save.rel(file), Save.rel(generated), nodes.size()
                );
            }
        }
        return total;
    }

    /**
     * Save this Java file.
     * @param java The XML with Java
     * @param generated Path to all files
     * @throws IOException If fails
     */
    private static void saveJava(final XML java, final Path generated) throws IOException {
        final String type = java.xpath("@java-name").get(0);
        final Path dest = new Place(type).make(
            generated, "java"
        );
        new Save(
            new Joined(
                "",
                java.xpath("java/text()")
            ),
            dest
        ).save();
    }

    /**
     * XML validated for errors.
     * @since 1.0
     */
    private static class EnsureNoErrors {

        /**
         * XML to validate.
         */
        private final XML xml;

        /**
         * Program name.
         */
        private final String program;

        /**
         * Severities to fail on.
         */
        private final Set<ErrorSeverity> failures;

        /**
         * Ctor.
         * @param xml XML to validate
         * @param program Program name to assign with errors
         * @param failures Set of severities to fail on
         */
        EnsureNoErrors(final XML xml, final String program, final Set<ErrorSeverity> failures) {
            this.xml = xml;
            this.program = program;
            this.failures = failures;
        }

        /**
         * Validate XML. Raise exceptions if needed.
         * @return Original XML
         */
        public XML validate() {
            final List<XML> errors = this.xml.nodes("/program/errors/error");
            for (final XML error : errors) {
                final ErrorSeverity svty = severity(error);
                if (svty.equals(ErrorSeverity.ERROR)) {
                    this.log(error, Logger::error);
                } else if (
                    ErrorSeverity.WARNING.equals(svty)
                    || ErrorSeverity.ADVICE.equals(svty)) {
                    this.log(error, Logger::warn);
                }
            }
            this.fail(
                errors.stream()
                    .collect(Collectors.groupingBy(EnsureNoErrors::severity))
            );
            return this.xml;
        }

        /**
         * Extract severity from error node.
         * @param error Error node XML
         * @return Extracted severity
         */
        private static ErrorSeverity severity(final XML error) {
            final List<String> svts = error.xpath("@severity");
            final ErrorSeverity svty;
            if (svts.isEmpty()) {
                svty = ErrorSeverity.ERROR;
            } else {
                svty = new EsBase(svts.get(0));
            }
            return svty;
        }

        /**
         * Fail if needed for provided errors.
         * @param errors Severity to errors map
         */
        private void fail(final Map<ErrorSeverity, List<XML>> errors) {
            for (final ErrorSeverity severity: this.failures) {
                if (errors.containsKey(severity)
                    && !errors.get(severity).isEmpty()) {
                    throw new IllegalStateException(
                        String.format(
                            "There are %d %s(s) in %s, see log above",
                            errors.get(severity).size(),
                            severity.asText(),
                            this.program
                        )
                    );
                }
            }
        }

        /**
         * Log error with given log function.
         * @param error Error to log
         * @param logfunc Log function
         */
        private void log(final XML error, final LogFunction logfunc) {
            logfunc.log(
                this,
                "[%s:%s] %s (%s:%s)",
                this.program,
                error.xpath("@line").get(0),
                error.xpath("text()").get(0),
                error.xpath("@check").get(0),
                error.xpath("@step").get(0)
            );
        }

        /**
         * Functional interface for logging function.
         * {@link Logger#warn(Object, String, Object...)},
         * {@link Logger#error(Object, String, Object...)}, etc
         * @since 1.0
         */
        private interface LogFunction {

            /**
             * Write log message.
             * @param source Producer of the log message
             * @param msg Log message
             * @param args Log message params
             */
            void log(Object source, String msg, Object... args);
        }
    }

}
