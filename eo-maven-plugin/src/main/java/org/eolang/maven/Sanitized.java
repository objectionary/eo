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
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Sanitized XMIR files.
 * @since 1.0
 */
public class Sanitized {
    /**
     * Error severity.
     */
    public static final String ERROR = "error";

    /**
     * Warning severity.
     */
    public static final String WARNING = "warning";

    /**
     * Advice severity.
     */
    public static final String ADVICE = "advice";

    /**
     * XML to validate.
     */
    private final Path source;

    /**
     * Ctor.
     * @param source Path to XMIR to validate
     */
    public Sanitized(final Path source) {
        this.source = source;
    }

    /**
     * Check for errors.
     * @param failures Severities to fail on
     * @throws FileNotFoundException If source file is missing
     */
    public void sanitize(final Set<String> failures) throws FileNotFoundException {
        final XML xml = new XMLDocument(this.source);
        final List<XML> errors = xml.nodes("/program/errors/error");
        final String program = xml.xpath("/program/@name").get(0);
        for (final XML error : errors) {
            final String svty = severity(error);
            if (svty.equals(Sanitized.ERROR)) {
                this.log(error, program, Logger::error);
            } else if (
                Sanitized.WARNING.equals(svty)
                    || Sanitized.ADVICE.equals(svty)) {
                this.log(error, program, Logger::warn);
            }
        }
        this.fail(
            errors.stream()
                .collect(Collectors.groupingBy(Sanitized::severity)),
            program,
            failures
        );
    }

    /**
     * Extract severity from error node.
     * @param error Error node XML
     * @return Extracted severity
     */
    private static String severity(final XML error) {
        final List<String> svts = error.xpath("@severity");
        final String svty;
        if (svts.isEmpty()) {
            svty = Sanitized.ERROR;
        } else {
            svty = svts.get(0);
        }
        return svty;
    }

    /**
     * Fail if needed for provided errors.
     *
     * @param errors Severity to errors map
     * @param program Program being sanitized
     * @param failures Severities to fail on
     */
    private void fail(final Map<String, List<XML>> errors,
        final String program,
        final Set<String> failures) {
        for (final String severity: failures) {
            if (errors.containsKey(severity)
                && !errors.get(severity).isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "There are %d %s(s) in %s, see log above",
                        errors.get(severity).size(),
                        severity,
                        program
                    )
                );
            }
        }
    }

    /**
     * Log error with given log function.
     * @param error Error to log
     * @param program Program being sanitized
     * @param logfunc Log function
     */
    private void log(final XML error, final String program, final Sanitized.LogFunction logfunc) {
        logfunc.log(
            this,
            "[%s:%s] %s (%s:%s)",
            program,
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
