/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
public final class FpIfTargetOlder extends FpEnvelope {
    /**
     * Ctor.
     * @param destination Function that modifies result target path
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfTargetOlder(
        final Func<Path, Path> destination, final Footprint first, final Footprint second
    ) {
        super(
            new FpFork(
                (source, target) -> {
                    final Path dest = destination.apply(target);
                    final boolean older = FpIfTargetOlder.isAfter(dest, source);
                    if (older) {
                        Logger.debug(
                            FpIfTargetOlder.class,
                            "Target file %[file]s is older than source %[file]s",
                            dest, source
                        );
                    } else {
                        Logger.debug(
                            FpIfTargetOlder.class,
                            "Target file %[file]s is newer than source %[file]s",
                            dest, source
                        );
                    }
                    return older;
                },
                first,
                second
            )
        );
    }

    /**
     * Returns true if first given path is older in terms of last modified time.
     * @param first First path to compare
     * @param second Second path to compare
     * @return True if first path is older that second path
     * @throws IOException If fails to compare files
     */
    private static boolean isAfter(final Path first, final Path second) throws IOException {
        return Files.getLastModifiedTime(first).toInstant().isAfter(
            Files.getLastModifiedTime(second).toInstant()
        );
    }
}
