/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Compile and lint all EO files.
 *
 * @since 0.52
 */
@Mojo(
    name = "compile",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class CompileMojo extends SafeMojo {
    /**
     * Mojas to execute.
     */
    private static final Moja<?>[] MOJAS = {
        new Moja<>(AssembleMojo.class),
        new Moja<>(LintMojo.class),
        new Moja<>(ResolveMojo.class),
        new Moja<>(PlaceMojo.class),
    };

    @Override
    public void exec() {
        final long begin = System.currentTimeMillis();
        for (final Moja<?> moja : CompileMojo.MOJAS) {
            moja.copy(this).execute();
        }
        Logger.info(
            this,
            "Compilation process took %[ms]s",
            System.currentTimeMillis() - begin
        );
    }
}
