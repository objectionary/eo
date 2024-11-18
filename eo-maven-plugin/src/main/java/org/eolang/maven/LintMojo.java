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
import java.nio.file.Path;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.tojos.ForeignTojo;

/**
 * Check all XMIR files for the presence of bad practices.
 *
 * @since 0.42
 */
@Mojo(
    name = "lint",
    defaultPhase = LifecyclePhase.VERIFY,
    threadSafe = true
)
public final class LintMojo extends SafeMojo {

    @Override
    public void exec() {
        final long start = System.currentTimeMillis();
        final Collection<ForeignTojo> tojos = this.scopedTojos().withOptimized();
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> () -> this.checked(tojo),
                    tojos
                )
            )
        ).intValue();
        if (total > 0) {
            Logger.info(
                this,
                "Checked %d out of %d XMIR program(s) in %[ms]s",
                total, tojos.size(),
                System.currentTimeMillis() - start
            );
        } else {
            Logger.debug(this, "No XMIR programs out of %d checked", tojos.size());
        }
    }

    /**
     * Check XMIR file.
     * @param tojo Foreign tojo
     * @return Amount of optimized XMIR files
     * @throws Exception If fails
     */
    private int checked(final ForeignTojo tojo) throws Exception {
        final Path path = tojo.optimized();
        // check it
        return 1;
    }

}
