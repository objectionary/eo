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
package org.eolang.maven.footprint;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;

/**
 * Version of the cache.
 * The version consists of two main components:
 *  1) Base, which is maven-style version string
 *  2) Hash string, containing git hash.
 * The two parts define the cacheable location by method {@link #path}.
 * If base is empty or contains '0.0.0' or 'SNAPSHOT' (see {@link CacheVersion#NOT_CACHEABLE}
 * versions) the version is considered non-cacheable.
 *
 * @since 0.30
 */
public final class CacheVersion {

    /**
     * Not cacheable versions.
     */
    private static final String[] NOT_CACHEABLE = {"0.0.0", "SNAPSHOT"};

    /**
     * Version of the eo-maven-plugin which currently builds a program.
     * Version could be:
     *  - 0.0.0 - if the version is not set in the pom.xml
     *  - 0.30.0 - if the version is set in the pom.xml
     *  - 1.0-SNAPSHOT - if the not stable development version is set in the pom.xml
     *  By some reason, the version could also be empty.
     */
    private final String version;

    /**
     * Hash of tag from the objectionary/home.
     */
    private final String hash;

    /**
     * Ctor.
     * @param ver Version of the eo-maven-plugin which currently builds a program.
     * @param hsh Hash of the objectionary tag.
     */
    public CacheVersion(final String ver, final String hsh) {
        this.version = ver;
        this.hash = hsh;
    }

    /**
     * Checks if the version is cacheable.
     * @return True if cacheable, false otherwise.
     */
    boolean cacheable() {
        return Objects.nonNull(this.hash)
            && !this.hash.isEmpty()
            && (Objects.isNull(this.version)
            || Arrays.stream(CacheVersion.NOT_CACHEABLE).noneMatch(this.version::contains));
    }

    /**
     * Get cache path.
     * @return Path.
     */
    Path path() {
        return Paths.get(Objects.toString(this.version, ""))
            .resolve(Objects.toString(this.hash, ""));
    }
}
