/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
package org.eolang.maven;

import java.nio.file.Path;

/**
 * Class for building footprint.
 * @since 0.28
 */
public class Footprints {
    /**
     * Type of Footprint.
     */
    private boolean iscached;

    /**
     * Version.
     */
    private String version;

    /**
     * Version tag.
     */
    private String hash;

    /**
     * Path to cache root.
     */
    private Path cache;

    /**
     * Main.
     */
    private Path main;

    /**
     * Set cached mode.
     *
     * @return This
     */
    public Footprints cached() {
        this.iscached = true;
        return this;
    }

    /**
     * Set hash.
     *
     * @param hsh Hash of tag
     * @return This
     */
    public Footprints withHash(final String hsh) {
        this.hash = hsh;
        return this;
    }

    /**
     * Set tag.
     *
     * @param ver Version of maven-plugin
     * @return This
     */
    public Footprints withVersion(final String ver) {
        this.version = ver;
        return this;
    }

    /**
     * Set main.
     *
     * @param dir Main
     * @return This
     */
    public Footprints withMain(final Path dir) {
        this.main = dir;
        return this;
    }

    /**
     * Set path.
     *
     * @param path Path
     * @return This
     */
    public Footprints withCache(final Path path) {
        this.cache = path;
        return this;
    }

    /**
     * Return built instance.
     *
     * @return Built footprint.
     */
    public Footprint build() {
        final Footprint footprint;
        if (this.iscached) {
            footprint = new FtCached(
                new String[]{this.hash, this.version}, this.main, this.cache
            );
        } else {
            footprint = new FtDefault(this.main);
        }
        return footprint;
    }

}
