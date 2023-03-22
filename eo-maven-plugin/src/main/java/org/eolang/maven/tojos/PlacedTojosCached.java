/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven.tojos;

import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * Placed tojos that cached in memory.
 * After implementing the <a href="https://github.com/yegor256/tojos/issues/60">issue</a>
 * the class will be removed.
 * @since 0.30
 */
public final class PlacedTojosCached {

    /**
     * Delegate.
     */
    private final PlacedTojos origin;

    /**
     * Cached placed tojos.
     */
    private final Unchecked<Map<String, PlacedTojo>> cache;

    /**
     * Ctor.
     * @param tojos Placed tojos.
     */
    public PlacedTojosCached(final PlacedTojos tojos) {
        this.origin = tojos;
        this.cache = new Unchecked<>(new Sticky<>(() -> PlacedTojosCached.placedCache(tojos)));
    }

    /**
     * Find placed tojo by path.
     * @param target Path.
     * @return Placed tojo.
     */
    public Optional<PlacedTojo> find(final Path target) {
        return Optional.ofNullable(this.cache.value().get(target.toString()));
    }

    /**
     * Place class.
     * @param target Path.
     * @param related Related path.
     * @param dep Dependency.
     */
    public void placeClass(final Path target, final String related, final String dep) {
        final PlacedTojo tojo = this.origin.placeClass(target, related, dep);
        this.cache.value().putIfAbsent(target.toString(), tojo);
    }

    /**
     * Collect all binaries into a fast map for efficient search.
     * @param tojos Placed tojos.
     * @return Cached placed tojos.
     * @todo #1894:30min Replace PlaceMojo#placedCache crutch with an appropriate solution from
     *  Tojos library. The original problem is that Tojos has not so optimal reading mechanism
     *  and it's not efficient to read row by row from tojos. You can check the progress
     *  <a href="https://github.com/yegor256/tojos/issues/60">here</a>.
     */
    private static Map<String, PlacedTojo> placedCache(final PlacedTojos tojos) {
        return tojos.classes()
            .stream()
            .collect(
                Collectors.toMap(
                    PlacedTojo::identifier,
                    row -> row
                )
            );
    }
}
