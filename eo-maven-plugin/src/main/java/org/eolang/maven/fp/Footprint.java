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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.Func;
import org.cactoos.func.StickyFunc;
import org.cactoos.text.TextOf;

/**
 * Footprint is a function that knows how to properly copy content from source
 * file to target file.
 * <p>Footprint works with three places:
 *  1) source file which is used to extract content from it
 *  2) target file which is used to store content to it
 *  3) global cache file which is used in order not to extract content from source
 *     if it's already exists
 *  </p>
 *
 * <p>General statements:
 *  1) if target older than source - no extraction is happened
 *  2) if target younger than source or does not exist - it will be created and filled up.
 *     It can be created from source, or from global cache if it exists and cacheable and
 *     older than source.
 *  3) the cache is updated if it's cacheable AND (it does not exist or if it's younger than source)
 *  </p>
 *
 * <p>Excluding any type of errors there are 4 possible scenarios of Footprint work:
 *  1) do nothing and just return target file.
 *  2) extract content, update target and return target. It can be possible if target younger than
 *  3) extract content, update target, update cache and return target
 *  4) copy content from cache to target and return target</p>
 * @since 0.41.0
 */
public final class Footprint implements Func<Func<Path, String>, Path> {
    /**
     * Absolute path to file with source code.
     */
    private final Path source;

    /**
     * Absolute path to target file.
     */
    private final Path target;

    /**
     * Global cache.
     */
    private final Cache cache;

    /**
     * Ctor.
     * @param source Absolute source path
     * @param target Absolute target path
     * @param cache Cache
     */
    public Footprint(
        final Path source,
        final Path target,
        final Cache cache
    ) {
        this.source = source;
        this.target = target;
        this.cache = cache;
    }

    @Override
    public Path apply(final Func<Path, String> content) throws Exception {
        if (!Footprint.exists(this.source)) {
            throw new IllegalStateException(
                String.format("Source file %s does not exist", this.source)
            );
        }
        final Path result;
        if (Footprint.exists(this.target)) {
            if (Footprint.isAfter(this.target, this.source)) {
                result = this.target;
            } else {
                result = this.dependentOnCache(content);
            }
        } else {
            result = this.dependentOnCache(content);
        }
        return result;
    }

    /**
     * Updates target depends on global cache.
     * @param content Content function
     * @return Path to target
     * @throws IOException If fails to check files last modified time
     * @checkstyle NestedIfDepthCheck (30 lines)
     */
    private Path dependentOnCache(final Func<Path, String> content) throws Exception {
        final Path result;
        if (this.cache.cacheable()) {
            final Path cached = this.cache.path();
            if (Footprint.exists(cached)) {
                if (Footprint.isAfter(cached, this.source)) {
                    result = this.copiedFromCache();
                } else {
                    result = this.updatedAll(content);
                }
            } else {
                result = this.updatedAll(content);
            }
        } else {
            result = this.updatedTarget(content);
        }
        return result;
    }

    /**
     * Takes content given function and update target file.
     * @param content Content function
     * @return Absolute path to target file
     */
    private Path updatedTarget(final Func<Path, String> content) throws Exception {
        return Footprint.updated(this.source, this.target, content);
    }

    /**
     * Takes content from given function, updates target file, updates global cache file.
     * @param content Content function
     * @return Absolute path to target file
     * @throws Exception If failed to extract content or update files
     */
    private Path updatedAll(final Func<Path, String> content) throws Exception {
        final Func<Path, String> sticky = new StickyFunc<>(content);
        final Path result = this.updatedTarget(sticky);
        Footprint.updated(this.source, this.cache.path(), sticky);
        return result;
    }

    /**
     * Takes content from global cache and updates target file.
     * @return Absolute path to target file
     */
    private Path copiedFromCache() throws Exception {
        return Footprint.updated(
            this.cache.path(),
            this.target,
            src -> new TextOf(src).asString()
        );
    }

    /**
     * Takes content from given function and updates given file.
     * @param source Absolute path to source file
     * @param dest Absolute path to file to update
     * @param content Content function
     * @return Absolute path to file to update
     * @throws Exception If failed to extract content or update files
     */
    private static Path updated(
        final Path source, final Path dest, final Func<Path, String> content
    ) throws Exception {
        new Saved.Default(content.apply(source), dest).value();
        return dest;
    }

    /**
     * Returns True if file by given path exists.
     * @param path Path to check
     * @return True if file exists
     */
    private static boolean exists(final Path path) {
        return path.toFile().exists();
    }

    /**
     * Returns True if last modified time of the first file more than last modified.
     * time of the second file
     * @param first First file
     * @param second Second file
     * @return True if first is after second
     * @throws IOException If failed to check last modified time
     */
    private static boolean isAfter(final Path first, final Path second) throws IOException {
        return Files.getLastModifiedTime(first).toInstant().isAfter(
            Files.getLastModifiedTime(second).toInstant()
        );
    }
}
