/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import org.cactoos.Func;

/**
 * Footprint that behaves like one of the given wrapped footprints depending on
 * existence of provided target path.
 * @since 0.41
 */
final class FpIfTargetExists extends FpEnvelope {
    /**
     * Ctor.
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfTargetExists(final Footprint first, final Footprint second) {
        this(target -> target, first, second);
    }

    /**
     * Ctor.
     * @param destination Function that modifies result target path
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfTargetExists(
        final Func<Path, Path> destination, final Footprint first, final Footprint second
    ) {
        super(
            new FpFork(
                (source, target) -> {
                    final Path dest = destination.apply(target);
                    final boolean exists = dest.toFile().exists();
                    if (!exists) {
                        Logger.debug(
                            FpIfTargetExists.class, "Target file %[file]s does not exist", dest
                        );
                    }
                    return exists;
                },
                first,
                second
            )
        );
    }
}
