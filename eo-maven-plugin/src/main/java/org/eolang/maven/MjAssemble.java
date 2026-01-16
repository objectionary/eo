/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Pull all necessary EO XML files from Objectionary and parse them all.
 *
 * <p>
 *     This goal combines {@link MjParse}, {@link MjProbe} and {@link MjPull}.
 *     See their documentation to find out more details.
 *     The {@link MjAssemble} runs these goals repeatedly until no new objects are
 *     pulled or parsed in a cycle.
 * </p>
 *
 * @since 0.1
 */
@Mojo(
    name = "assemble",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjAssemble extends MjSafe {
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
        new Moja<>(MjParse.class),
        new Moja<>(MjProbe.class),
        new Moja<>(MjPull.class),
    };

    @Override
    public void exec() {
        final long begin = System.currentTimeMillis();
        String before = this.scopedTojos().status();
        int cycle = 0;
        while (true) {
            final long start = System.currentTimeMillis();
            for (final Moja<?> moja : MjAssemble.MOJAS) {
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
