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
import java.nio.file.Path;
import java.util.Arrays;
import java.util.function.Supplier;

/**
 * Footprint that uses cache if it's cacheable.
 * Does not use and updates cache at all otherwise.
 * @since 0.41
 * @checkstyle ParameterNumberCheck (100 lines)
 */
public final class FpOnlyReleases extends FpEnvelope {
    /**
     * Not cacheable versions.
     */
    private static final String[] NOT_CACHEABLE = {"0.0.0", "SNAPSHOT"};

    /**
     * Ctor.
     * @param origin Original wrapped footprint that updates target from source
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache directory
     * @param hash Git hash as part of absolute cache directory
     * @param tail Last part of absolute cache directory
     */
    public FpOnlyReleases(
        final Footprint origin,
        final Path base,
        final String semver,
        final String hash,
        final Path tail
    ) {
        this(origin, base, semver, () -> hash, tail);
    }

    /**
     * Ctor.
     * @param origin Original wrapped footprint that updates target from source
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache directory
     * @param hash Git hash as part of absolute cache directory
     * @param tail Last part of absolute cache directory
     */
    public FpOnlyReleases(
        final Footprint origin,
        final Path base,
        final String semver,
        final Supplier<String> hash,
        final Path tail
    ) {
        super(
            new FpFork(
                (source, target) -> {
                    final String hsh = hash.get();
                    final boolean cacheable = !hsh.isEmpty()
                        && Arrays.stream(FpOnlyReleases.NOT_CACHEABLE).noneMatch(semver::contains);
                    if (cacheable) {
                        Logger.debug(
                            FpOnlyReleases.class,
                            "Cache with version '%s' and hash '%s' is cacheable, using it",
                            semver, hsh
                        );
                    } else {
                        Logger.debug(
                            FpOnlyReleases.class,
                            "Cache with version '%s' and hash '%s' is not cacheable, skipping it",
                            semver, hsh
                        );
                    }
                    return cacheable;
                },
                new FpViaCache(
                    origin,
                    () -> base.resolve(semver).resolve(hash.get()).resolve(tail)
                ),
                origin
            )
        );
    }
}
