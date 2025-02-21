/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Pull all necessary EO XML files from Objectionary and parse them all.
 *
 * @since 0.1
 */
@Mojo(
    name = "assemble",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class AssembleMojo extends SafeMojo {
    /**
     * The intermediate representation extension.
     */
    static final String XMIR = "xmir";

    /**
     * Source file extension.
     */
    static final String EO = "eo";

    /**
     * Mojas to execute.
     */
    private static final Moja<?>[] MOJAS = {
        new Moja<>(ParseMojo.class),
        new Moja<>(ShakeMojo.class),
        new Moja<>(ProbeMojo.class),
        new Moja<>(PullMojo.class),
        new Moja<>(ResolveMojo.class),
        new Moja<>(MarkMojo.class),
        new Moja<>(PlaceMojo.class),
    };

    @Override
    public void exec() {
        final long begin = System.currentTimeMillis();
        String before = this.scopedTojos().status();
        int cycle = 0;
        while (true) {
            final long start = System.currentTimeMillis();
            for (final Moja<?> moja : AssembleMojo.MOJAS) {
                moja.copy(this).execute();
            }
            final String after = this.scopedTojos().status();
            ++cycle;
            if (after.equals(before)) {
                Logger.info(
                    this, "Last assemble cycle #%d (%s), took %[ms]s",
                    cycle, after, System.currentTimeMillis() - start
                );
                break;
            } else {
                Logger.info(
                    this, "Assemble cycle #%d (%s -> %s), took %[ms]s",
                    cycle, before, after, System.currentTimeMillis() - start
                );
            }
            before = after;
        }
        Logger.info(
            this,
            "%d assemble cycle(s) produced some new object(s) in %[ms]s: %s",
            cycle,
            System.currentTimeMillis() - begin,
            before
        );
    }
}
