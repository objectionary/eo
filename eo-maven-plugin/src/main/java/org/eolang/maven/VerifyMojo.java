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
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import java.nio.file.Path;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.optimization.OptTrain;
import org.eolang.maven.optimization.Optimization;
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
        final Optimization optimization = this.optimization();
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> () -> this.verified(tojo, optimization),
                    new Filtered<>(
                        ForeignTojo::notVerified,
                        tojos
                    )
                )
            )
        ).intValue();
        if (total > 0) {
            Logger.info(
                this,
                "Verified %d out of %d XMIR program(s)", total,
                tojos.size()
            );
        } else if (tojos.isEmpty()) {
            Logger.info(this, "There are no XMIR programs, nothing to verify");
        } else {
            Logger.info(this, "No XMIR programs out of %d verified", tojos.size());
        }
    }

    /**
     * XMIR verified to another XMIR.
     * @param tojo Foreign tojo
     * @param optimization Verification optimization
     * @return Amount of verified tojos
     * @throws Exception If failed to verify
     */
    private int verified(final ForeignTojo tojo, final Optimization optimization) throws Exception {
        final Path source = tojo.shaken();
        final XML xmir = new XMLDocument(source);
        final String name = xmir.xpath("/program/@name").get(0);
        final Path base = this.targetDir.toPath().resolve(VerifyMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        tojo.withVerified(
            new FpDefault(
                src -> optimization.apply(xmir).toString(),
                this.cache.toPath().resolve(VerifyMojo.CACHE),
                this.plugin.getVersion(),
                new TojoHash(tojo),
                base.relativize(target)
            ).apply(source, target)
        );
        return 1;
    }

    /**
     * Verifying optimizations for tojos.
     *
     * @return Verifying optimizations
     */
    private Optimization optimization() {
        Optimization opt = new OptTrain(
            this::logErrors,
            new TrClasspath<>(
                new TrDefault<>(),
                "/org/eolang/parser/fail-on-errors.xsl",
                "/org/eolang/parser/fail-on-critical.xsl"
            ).back()
        );
        if (this.failOnWarning) {
            opt = new OptTrain(opt, "/org/eolang/parser/fail-on-warnings.xsl");
        }
        return opt;
    }

    /**
     * Log errors of xml.
     * @param xml XMIR.
     * @return XML.
     */
    private XML logErrors(final XML xml) {
        for (final XML error : xml.nodes("/program/errors/error")) {
            final String message = Logger.format(
                "%[file]s, line %s: %s",
                xml.xpath("/program/@source").get(0),
                error.xpath("@line").get(0),
                error.xpath("text()").get(0)
            );
            final String severity = error.xpath("@severity").get(0);
            switch (severity) {
                case "warning":
                    Logger.warn(this, message);
                    break;
                case "error":
                case "critical":
                    Logger.error(this, message);
                    break;
                default:
                    throw new IllegalArgumentException(
                        String.format("Incorrect severity: %s", severity)
                    );
            }
        }
        return xml;
    }
}
