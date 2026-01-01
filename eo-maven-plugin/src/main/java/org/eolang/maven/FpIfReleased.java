/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.util.function.Supplier;

/**
 * Footprint that behaves like one of the given footprints depending on
 * hash and semver of provided cache.
 * Similar to {@link FpFork} but the condition is based on versioning and hash.
 * If the version is not a released one (e.g. "0.0.0" or "SNAPSHOT")
 * or the hash is empty, the second footprint is used, otherwise the first one.
 * @since 0.41
 * @checkstyle ParameterNumberCheck (100 lines)
 */
final class FpIfReleased extends FpEnvelope {

    /**
     * Ctor.
     * @param semver Cache version
     * @param hash Git hash
     * @param first First footprint to use if a version is released and a hash is present
     * @param second Second footprint to use if a version is not released or a hash is not present
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
     * @param first First footprint to use if a version is released and a hash is present
     * @param second Second footprint to use if a version is not released or a hash is not present
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
                    final boolean cacheable = !hsh.isEmpty();
                    if (cacheable) {
                        Logger.debug(
                            FpIfReleased.class,
                            "The version '%s' and hash '%s' are good, using cache for %[file]s",
                            semver, hsh, target
                        );
                    } else {
                        Logger.debug(
                            FpIfReleased.class,
                            "The version is '%s' but hash is absent, not using cache for %[file]s",
                            semver, target
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
