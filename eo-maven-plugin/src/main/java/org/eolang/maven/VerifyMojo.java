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
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.number.SumOf;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.TojoHash;

/**
 * Mojo that checks errors and warnings after "assemble" phase.
 *
 * @since 0.31.0
 */
@Mojo(
    name = "verify",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class VerifyMojo extends SafeMojo {
    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "6-verify";

    /**
     * Subdirectory for optimized cache.
     */
    static final String CACHE = "verified";

    /**
     * Whether we should fail on warning.
     *
     * @checkstyle MemberNameCheck (11 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(
        property = "eo.failOnWarning",
        required = true,
        defaultValue = "false"
    )
    private boolean failOnWarning;

    @Override
    void exec() {
        final Collection<ForeignTojo> tojos = this.scopedTojos().withShaken();
        final ConcurrentHashMap<String, Integer> counts = new ConcurrentHashMap<>();
        counts.putIfAbsent("critical", 0);
        counts.putIfAbsent("error", 0);
        counts.putIfAbsent("warning", 0);
        final Collection<ForeignTojo> must = new ListOf<>(
            new Filtered<>(
                ForeignTojo::notVerified,
                tojos
            )
        );
        final int passed = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> () -> this.verify(tojo, counts),
                    must
                )
            )
        ).intValue();
        if (must.isEmpty()) {
            Logger.info(this, "No XMIR programs out of %d verified", tojos.size());
        } else if (tojos.isEmpty()) {
            Logger.info(this, "There are no XMIR programs, nothing to verify");
        } else {
            final String sum = VerifyMojo.summary(counts);
            Logger.info(
                this,
                "Verified %d out of %d XMIR program(s) that needed verification (out of %d total programs): %s",
                passed, must.size(), tojos.size(), sum
            );
            if (counts.get("error") > 0 || counts.get("critical") > 0
                || counts.get("warning") > 0 && this.failOnWarning) {
                throw new IllegalStateException(
                    String.format("In %d XMIR files, we found %s", must.size(), sum)
                );
            }
        }
    }

    /**
     * XMIR verified to another XMIR.
     * @param tojo Foreign tojo
     * @param counts Counts of errors, warnings, and critical
     * @return Amount of passed tojos (1 if passed, 0 if errors)
     * @throws Exception If failed to verify
     */
    private int verify(final ForeignTojo tojo,
        final ConcurrentHashMap<String, Integer> counts) throws Exception {
        final Path source = tojo.shaken();
        final XML xmir = new XMLDocument(source);
        final String name = xmir.xpath("/program/@name").get(0);
        final Path base = this.targetDir.toPath().resolve(VerifyMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        int verified = 1;
        try {
            tojo.withVerified(
                new FpDefault(
                    src -> {
                        if (!this.good(xmir, counts)) {
                            throw new SkipException();
                        }
                        return xmir.toString();
                    },
                    this.cache.toPath().resolve(VerifyMojo.CACHE),
                    this.plugin.getVersion(),
                    new TojoHash(tojo),
                    base.relativize(target)
                ).apply(source, target)
            );
        } catch (final SkipException ex) {
            Logger.debug(this, "Failed to verify %[file]s", source);
            verified = 0;
        }
        return verified;
    }

    /**
     * Log errors of XMIR.
     * @param xml XMIR.
     * @param counts Counts of errors, warnings, and critical
     * @return TRUE if it's good
     */
    private boolean good(final XML xml, final ConcurrentHashMap<String, Integer> counts) {
        boolean good = true;
        for (final XML error : xml.nodes("/program/errors/error")) {
            final List<String> line = error.xpath("@line");
            final StringBuilder message = new StringBuilder()
                .append(Logger.format("%[file]s", xml.xpath("/program/@source").get(0)));
            if (!line.isEmpty()) {
                message.append(':').append(Integer.parseInt(line.get(0)));
            }
            message.append(' ')
                .append(error.xpath("text()").get(0))
                .append(" (")
                .append(error.xpath("@check").get(0))
                .append(')');
            final String severity = error.xpath("@severity").get(0);
            counts.compute(severity, (sev, before) -> before + 1);
            switch (severity) {
                case "warning":
                    Logger.warn(this, message.toString());
                    if (this.failOnWarning) {
                        good = false;
                    }
                    break;
                case "error":
                case "critical":
                    Logger.error(this, message.toString());
                    good = false;
                    break;
                default:
                    throw new IllegalArgumentException(
                        String.format("Incorrect severity: %s", severity)
                    );
            }
        }
        return good;
    }

    /**
     * Summarize the counts.
     * @param counts Counts of errors, warnings, and critical
     * @return Summary text
     */
    private static String summary(final ConcurrentHashMap<String, Integer> counts) {
        final StringBuilder sum = new StringBuilder(100);
        final int criticals = counts.get("critical");
        if (criticals > 0) {
            sum.append(criticals).append(" critical error");
            if (criticals > 1) {
                sum.append('s');
            }
        }
        final int errors = counts.get("error");
        if (errors > 0) {
            if (sum.length() > 0) {
                sum.append(", ");
            }
            sum.append(errors).append(" error");
            if (errors > 1) {
                sum.append('s');
            }
        }
        final int warnings = counts.get("warning");
        if (warnings > 0) {
            if (sum.length() > 0) {
                sum.append(", ");
            }
            sum.append(warnings).append(" warning");
            if (warnings > 1) {
                sum.append('s');
            }
        }
        if (sum.length() == 0) {
            sum.append("no complaints");
        }
        return sum.toString();
    }

    /**
     * If this file must be skipped.
     *
     * @since 0.43.0
     */
    public static class SkipException extends RuntimeException {
    }
}
