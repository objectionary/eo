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
import java.util.Arrays;
import java.util.function.Supplier;

/**
 * Footprint that behaves like one of the given footprints depending on
 * hash and semver of provided cache.
 * @since 0.41
 * @checkstyle ParameterNumberCheck (100 lines)
 */
public final class FpIfReleased extends FpEnvelope {
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
    public FpIfReleased(
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
    public FpIfReleased(
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
