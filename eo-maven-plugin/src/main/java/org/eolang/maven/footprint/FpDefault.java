/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
package org.eolang.maven.footprint;

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
 * 4) if the semver is "0.0.0" or "SNAPSHOT" ({@link FpIfReleased}) - the cache and target is always
 *    regenerated
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
public final class FpDefault extends FpEnvelope {
    /**
     * Ctor.
     * @param content Function that returns content from source
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     */
    public FpDefault(
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
    public FpDefault(
        final Func<Path, String> content,
        final Path base,
        final String semver,
        final Supplier<String> hash,
        final Path tail
    ) {
        this(content, semver, hash, new CachePath(base, semver, hash, tail));
    }

    /**
     * Ctor.
     * @param content Function that returns content from source
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param cache Lazy cache path
     */
    private FpDefault(
        final Func<Path, String> content,
        final String semver,
        final Supplier<String> hash,
        final Supplier<Path> cache
    ) {
        this(new FpUpdateBoth(new FpGenerated(content), cache), semver, hash, cache);
    }

    /**
     * Ctor.
     * @param generated Footprint that generates content and updates in locally and in cache
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
                            generated
                        )
                    ),
                    generated
                )
            )
        );
    }
}
