/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.Func;

/**
 * Footprint that behaves like one of the given wrapped footprints depending on
 * the result of comparison target and source in terms of last modified date.
 * @since 0.41
 */
public final class FpIfTargetActual extends FpEnvelope {
    /**
     * Ctor.
     * @param destination Function that modifies result target path
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfTargetActual(
        final Func<Path, Path> destination, final Footprint first, final Footprint second
    ) {
        super(
            new FpFork(
                (source, target) -> {
                    final Path dest = destination.apply(target);
                    final boolean actual = FpIfTargetActual.isNotBefore(dest, source);
                    if (actual) {
                        Logger.debug(
                            FpIfTargetActual.class,
                            "Target file %[file]s is actual toward source %[file]s",
                            dest, source
                        );
                    } else {
                        Logger.debug(
                            FpIfTargetActual.class,
                            "Target file %[file]s is expired toward source %[file]s",
                            dest, source
                        );
                    }
                    return actual;
                },
                first,
                second
            )
        );
    }

    /**
     * Returns true if first given path is actual in terms of last modified time.
     * @param first First path to compare
     * @param second Second path to compare
     * @return True if first path is actual toward second path
     * @throws IOException If fails to compare files
     */
    private static boolean isNotBefore(final Path first, final Path second) throws IOException {
        return !Files.getLastModifiedTime(first).toInstant().isBefore(
            Files.getLastModifiedTime(second).toInstant()
        );
    }
}
