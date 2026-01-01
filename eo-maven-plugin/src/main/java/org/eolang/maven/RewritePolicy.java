/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import org.cactoos.BiFunc;

/**
 * Rewrite policy.
 * @since 0.56.7
 */
final class RewritePolicy implements BiFunc<Path, Path, Boolean> {

    /**
     * Rewrite it?
     */
    private final boolean rewrite;

    /**
     * Target directory.
     */
    private final Path target;

    /**
     * Ctor.
     * @param rwrte Rewrite?
     * @param tgt Target directory
     */
    RewritePolicy(final boolean rwrte, final Path tgt) {
        this.rewrite = rwrte;
        this.target = tgt;
    }

    @Override
    public Boolean apply(final Path src, final Path result) throws Exception {
        if (this.rewrite) {
            Logger.debug(
                this,
                "Rewriting %[file]s because XMIR %[file]s was changed",
                result, this.target
            );
        }
        return this.rewrite;
    }
}
