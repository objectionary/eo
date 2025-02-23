/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.util.Arrays;
import java.util.function.Supplier;

/**
 * Footprint that behaves like one of the given footprints depending on
 * hash and semver of provided cache.
 * @since 0.41
 * @checkstyle ParameterNumberCheck (100 lines)
 */
final class FpIfReleased extends FpEnvelope {
    /**
     * Not cacheable versions.
     */
    private static final String[] NOT_CACHEABLE = {"0.0.0", "SNAPSHOT"};

    /**
     * Ctor.
     * @param semver Cache version
     * @param hash Git hash
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfReleased(
        final String semver,
        final String hash,
        final Footprint first,
        final Footprint second
    ) {
        this(semver, () -> hash, first, second);
    }

    /**
     * Ctor.
     * @param semver Cache version
     * @param hash Git hash
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfReleased(
        final String semver,
        final Supplier<String> hash,
        final Footprint first,
        final Footprint second
    ) {
        super(
            new FpFork(
                (source, target) -> {
                    final String hsh = hash.get();
                    final boolean cacheable = !hsh.isEmpty()
                        && Arrays.stream(FpIfReleased.NOT_CACHEABLE).noneMatch(semver::contains);
                    if (cacheable) {
                        Logger.debug(
                            FpIfReleased.class,
                            "Cache with version '%s' and hash '%s' is cacheable, using it",
                            semver, hsh
                        );
                    } else {
                        Logger.debug(
                            FpIfReleased.class,
                            "Cache with version '%s' and hash '%s' is not cacheable, skipping it",
                            semver, hsh
                        );
                    }
                    return cacheable;
                },
                first,
                second
            )
        );
    }
}
