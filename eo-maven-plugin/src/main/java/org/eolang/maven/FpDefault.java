/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.Func;

/**
 * Default footprint that covers all the scenarios of updating target
 * from source using cache.
 * <p>General statements:
 * 1) if target older than source - target is not updated
 * 2) if target younger than source or does not exist - it will be created and filled up.
 *    It can be created from source, or from global cache if it exists and cacheable and
 *    older than source.
 * 3) the cache is updated if it's cacheable (it does not exist or if it's younger than source)
 * 4) if the semver is "0.0.0" or "SNAPSHOT" ({@link FpIfReleased}) - the target is always
 *    regenerated and cache is not touched at all.
 * </p>
 *
 * <p>Excluding any type of errors there are 4 possible scenarios of this {@link Footprint} work:
 * 1) do nothing and just return target file.
 * 2) update target from source and return target.
 * 3) update target from source, update cache from target and return target.
 * 4) copy content from cache to target and return target.</p>
 * @since 0.41
 * @checkstyle ParameterNumberCheck (100 lines)
 */
final class FpDefault extends FpEnvelope {
    /**
     * Ctor.
     * @param content Function that returns content from source
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     */
    FpDefault(
        final Func<Path, String> content,
        final Path base,
        final String semver,
        final String hash,
        final Path tail
    ) {
        this(content, base, semver, () -> hash, tail);
    }

    /**
     * Ctor.
     * @param content Function that returns content from source
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     */
    FpDefault(
        final Func<Path, String> content,
        final Path base,
        final String semver,
        final Supplier<String> hash,
        final Path tail
    ) {
        this(new FpGenerated(content), base, semver, hash, tail);
    }

    /**
     * Ctor.
     * @param generated Footprint that generates content
     * @param base Base cache path
     * @param semver Cache version
     * @param hash Cache hash
     * @param tail Cache tail path
     */
    FpDefault(
        final Footprint generated,
        final Path base,
        final String semver,
        final Supplier<String> hash,
        final Path tail
    ) {
        this(
            generated,
            semver,
            hash,
            new CachePath(base, semver, hash, tail)
        );
    }

    /**
     * Ctor.
     * @param generated Footprint that generates content
     * @param semver Cache version
     * @param hash Cache hash
     * @param cache Lazy cache path
     */
    private FpDefault(
        final Footprint generated,
        final String semver,
        final Supplier<String> hash,
        final Supplier<Path> cache
    ) {
        super(
            new FpExistedSource(
                new FpIfReleased(
                    semver,
                    hash,
                    new FpIfOlder(
                        new FpIgnore(),
                        new FpIfOlder(
                            target -> cache.get(),
                            new FpUpdateFromCache(cache),
                            new FpUpdateBoth(generated, cache)
                        )
                    ),
                    generated
                )
            )
        );
    }
}
