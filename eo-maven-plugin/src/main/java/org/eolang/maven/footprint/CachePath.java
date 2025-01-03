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

/**
 * Function that builds full path to file in global cache.
 * @since 0.41
 */
public final class CachePath implements Supplier<Path> {
    /**
     * Cache base directory.
     */
    private final Path base;

    /**
     * Cache version.
     */
    private final String semver;

    /**
     * Git hash.
     */
    private final Supplier<String> hash;

    /**
     * Cache tail path.
     */
    private final Path tail;

    /**
     * Ctor.
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public CachePath(
        final Path base, final String semver, final String hash, final Path tail
    ) {
        this(base, semver, () -> hash, tail);
    }

    /**
     * Ctor.
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public CachePath(
        final Path base, final String semver, final Supplier<String> hash, final Path tail
    ) {
        this.base = base;
        this.semver = semver;
        this.hash = hash;
        this.tail = tail;
    }

    @Override
    public Path get() {
        return this.base.resolve(this.semver).resolve(this.hash.get()).resolve(this.tail);
    }
}
