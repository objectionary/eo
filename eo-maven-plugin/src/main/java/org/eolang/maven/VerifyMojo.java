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
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import java.nio.file.Path;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Filtered;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
import org.eolang.maven.optimization.OptTrain;
import org.eolang.maven.optimization.Optimization;
import org.eolang.maven.tojos.ForeignTojo;

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
        final Collection<ForeignTojo> tojos = this.scopedTojos().withXmir();
        final int total = new OptimizedTojos(
            new Filtered<>(
                ForeignTojo::notVerified,
                tojos
            ),
            this.optimization(),
            new OptimizationTask(
                new MapOf<String, Path>(
                    new MapEntry<>(OptimizationFolder.TARGET.key(), this.targetDir.toPath()),
                    new MapEntry<>(OptimizationFolder.CACHE.key(), this.cache)
                ),
                new MapOf<String, String>(
                    new MapEntry<>(OptimizationFolder.TARGET.key(), "6-verify"),
                    new MapEntry<>(OptimizationFolder.CACHE.key(), "verified")
                ),
                ForeignTojo::withVerified,
                ForeignTojo::shaken
            )
        ).count();
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
     * Verifying optimizations for tojos.
     *
     * @return Verifying optimizations
     */
    private Optimization optimization() {
        Optimization opt = new OptTrain(
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

}
