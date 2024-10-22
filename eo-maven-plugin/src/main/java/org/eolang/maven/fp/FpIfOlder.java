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
package org.eolang.maven.fp;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.Func;

/**
 * Footprint that behaves as first given wrapped {@link Footprint}
 * if provided target is older than source.
 * Behaves as second given wrapped {@link Footprint} otherwise.
 * @since 0.41
 */
public final class FpIfOlder extends FpEnvelope {
    /**
     * Ctor.
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    public FpIfOlder(final Footprint first, final Footprint second) {
        this(target -> target, first, second);
    }

    /**
     * Ctor.
     * @param destination Function that modifies result target path
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    public FpIfOlder(
        final Func<Path, Path> destination,
        final Footprint first,
        final Footprint second
    ) {
        super(
            new FpFork(
                (source, target) -> {
                    final Path dest = destination.apply(target);
                    final boolean result;
                    final boolean exists = dest.toFile().exists();
                    if (exists) {
                        result = FpIfOlder.isAfter(dest, source);
                        if (result) {
                            Logger.debug(
                                FpIfOlder.class,
                                "Target file %[file]s is older than source %[file]s",
                                dest, source
                            );
                        } else {
                            Logger.debug(
                                FpIfOlder.class,
                                "Target file %[file]s is newer than source %[file]s",
                                dest, source
                            );
                        }
                    } else {
                        result = false;
                        Logger.debug(
                            FpIfOlder.class, "Target file %[file]s does not exist", dest
                        );
                    }
                    return result;
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
