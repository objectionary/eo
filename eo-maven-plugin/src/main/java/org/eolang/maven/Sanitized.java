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
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.eolang.parser.ErrorSeverity;

/**
 * Sanitized XMIR files.
 * @since 1.0
 */
public class Sanitized {
    /**
     * XML to validate.
     */
    private final Path source;

    /**
     * Severities to fail on.
     */
    private final Set<ErrorSeverity> failures;

    /**
     * Ctor.
     * @param source Path to XMIR to validate
     * @param failures Set of severities to fail on
     */
    public Sanitized(final Path source, final Set<ErrorSeverity> failures) {
        this.source = source;
        this.failures = failures;
    }

    /**
     * Check for errors.
     * @throws FileNotFoundException If source file is missing
     */
    public void sanitize() throws FileNotFoundException {
        final XML xml = new XMLDocument(this.source);
        final List<XML> errors = xml.nodes("/program/errors/error");
        final String program = xml.xpath("/program/@name").get(0);
        for (final XML error : errors) {
            final ErrorSeverity svty = severity(error);
            if (svty.equals(ErrorSeverity.ERROR)) {
                this.log(error, program, Logger::error);
            } else if (
                ErrorSeverity.WARNING.equals(svty)
                    || ErrorSeverity.ADVICE.equals(svty)) {
                this.log(error, program, Logger::warn);
            }
        }
        this.fail(
            errors.stream()
                .collect(Collectors.groupingBy(Sanitized::severity)),
            program
        );
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
            svty = new ErrorSeverity.EsBase(svts.get(0));
        }
        return svty;
    }

    /**
     * Fail if needed for provided errors.
     * @param errors Severity to errors map
     * @param program Program being sanitized
     */
    private void fail(final Map<ErrorSeverity, List<XML>> errors, final String program) {
        for (final ErrorSeverity severity: this.failures) {
            if (errors.containsKey(severity)
                && !errors.get(severity).isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "There are %d %s(s) in %s, see log above",
                        errors.get(severity).size(),
                        severity.asText(),
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
