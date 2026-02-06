/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.Func;
import org.cactoos.io.InputOf;

/**
 * Default footprint that covers all the scenarios of updating target
 * from source using cache.
 * <p>General statements:
 * 1) if target is newer than source - target is not updated
 * 2) if target is older than source or does not exist - it will be created and filled.
 *    It can be created from source, or from global cache if cache exists, is cacheable, and
 *    is newer than source.
 * 3) the cache is updated if it is cacheable (it does not exist or if it is older than source)
 * </p>
 * <p>Excluding any type of errors there are 4 possible scenarios
 * of this {@link Footprint} work:
 * 1) do nothing and just return target file.
 * 2) update target from source and return target.
 * 3) update target from source, update cache from target and return target.
 * 4) copy content from cache to target and return target.</p>
 * @since 0.41
 * @todo #4851:60min Remove {@link FpDefault} from codebase.
 *  The {@link FpDefault} class is used only in tests.
 *  We should remove it and all tests that use it.
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
        this(content, base, semver, () -> hash, tail, true);
    }

    /**
     * Ctor.
     * @param content Function that returns content from source
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     * @param global Is global cache enabled or not
     */
    FpDefault(
        final Func<Path, String> content,
        final Path base,
        final String semver,
        final Supplier<String> hash,
        final Path tail,
        final boolean global
    ) {
        this(
            new FpGenerated(
                src -> new InputOf(content.apply(src))
            ),
            base,
            semver,
            hash,
            tail,
            global
        );
    }

    /**
     * Ctor.
     * @param generated Footprint that generates content
     * @param base Base cache path
     * @param semver Cache version
     * @param hash Cache hash
     * @param tail Cache tail path
     * @param global Is global cache enabled or not
     */
    FpDefault(
        final Footprint generated,
        final Path base,
        final String semver,
        final Supplier<String> hash,
        final Path tail,
        final boolean global
    ) {
        this(generated, hash, new CachePath(base, semver, hash, tail), global);
    }

    /**
     * Ctor.
     * <p>Here {@link FpIfReleased} is on the first place because we don't want to work with any
     * type of cache (local or global) if we work with SNAPSHOT version of the plugin</p>
     * @param generated Footprint that generates content
     * @param hash Cache hash
     * @param cache Lazy cache path
     * @param global Is global cache enabled or not
     */
    private FpDefault(
        final Footprint generated,
        final Supplier<String> hash,
        final Supplier<Path> cache,
        final boolean global
    ) {
        super(
            new FpExistedSource(
                new FpIfReleased(
                    hash,
                    new FpIfOlder(
                        new FpIgnore(),
                        new FpFork(
                            global,
                            new FpIfOlder(
                                target -> cache.get(),
                                new FpUpdateFromCache(cache),
                                new FpUpdateBoth(generated, cache)
                            ),
                            generated
                        )
                    ),
                    generated
                )
            )
        );
    }
}
