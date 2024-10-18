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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;

/**
 * Cache directory.
 * @since 0.41.0
 */
public final class Cache {
    /**
     * Base cache directory.
     */
    private final Path base;

    /**
     * Cache version.
     */
    private final Cache.Version version;

    /**
     * Relative path of base and version.
     */
    private final Path relative;

    /**
     * Ctor.
     * @param base Base cache directory
     * @param version Cache version
     * @param relative Relative path
     */
    public Cache(final Path base, final Cache.Version version, final Path relative) {
        this.base = base;
        this.version = version;
        this.relative = relative;
    }

    /**
     * Checks if the current cache directory is cacheable.
     * The directory is cacheable if cache version is cacheable.
     * @return True if cacheable
     */
    public boolean cacheable() {
        return this.version.cacheable();
    }

    /**
     * Return full absolute path to cache directory.
     * @return Path to cache directory
     */
    public Path path() {
        return this.base.resolve(this.version.path()).resolve(this.relative);
    }

    /**
     * Cache version.
     * The version consists of two main components:
     *  1) Base, which is maven-style version string
     *  2) Hash string, containing git hash.
     * The two parts define the cacheable location by method {@link #path}.
     * If base is empty or contains '0.0.0' or 'SNAPSHOT' (see {@link #NOT_CACHEABLE}
     * versions) the version is considered non-cacheable.
     * @since 0.41.0
     */
    public static class Version {
        /**
         * Not cacheable versions.
         */
        private static final String[] NOT_CACHEABLE = {"0.0.0", "SNAPSHOT"};

        /**
         * Version of the eo-maven-plugin which currently builds a program. Version could be:
         * - 0.0.0 - if the version is not set in the pom.xml
         * - 0.30.0 - if the version is set in the pom.xml
         * - 1.0-SNAPSHOT - if the not stable development version is set in the pom.xml By some
         * reason, the version could also be empty.
         */
        private final String semver;

        /**
         * Hash of tag from the objectionary/home.
         */
        private final String hash;

        /**
         * Ctor.
         * @param ver Version of the eo-maven-plugin which currently builds a program.
         * @param hsh Hash of the objectionary tag.
         */
        public Version(final String ver, final String hsh) {
            this.semver = ver;
            this.hash = hsh;
        }

        /**
         * Get cache path.
         * @return Path.
         */
        Path path() {
            return Paths.get(Objects.toString(this.semver, ""))
                .resolve(Objects.toString(this.hash, ""));
        }

        /**
         * Checks if the version is cacheable.
         * @return True if cacheable, false otherwise.
         */
        boolean cacheable() {
            return Objects.nonNull(this.hash)
                && !this.hash.isEmpty()
                && Objects.nonNull(this.semver)
                && Arrays.stream(Cache.Version.NOT_CACHEABLE).noneMatch(this.semver::contains);
        }
    }
}
