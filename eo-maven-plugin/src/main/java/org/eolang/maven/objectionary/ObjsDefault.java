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
package org.eolang.maven.objectionary;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.cactoos.Input;
import org.cactoos.Scalar;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.CommitHash;

/**
 * Default objectionaries.
 * The class is immutable, but NOT thread-safe.
 * @since 0.29.6
 */
public final class ObjsDefault implements Objectionaries {

    private final Unchecked<Path> cache;

    private final Scalar<Boolean> cached;

    /**
     * Hash-map.
     */
    private final Map<? super String, Objectionary> map;

    @SafeVarargs
    public ObjsDefault(final Map.Entry<CommitHash, Objectionary>... entries) {
        this(Arrays.stream(entries).collect(
            Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
    }

    public ObjsDefault(
        final Scalar<Path> cache,
        final Scalar<Boolean> cached
    ) {
        this(cache, cached, new HashMap<>(0));
    }

    /**
     * Constructor for tests.
     * @param ojs Predefined Objectionaries.
     */
    private ObjsDefault(final Map<? super String, Objectionary> ojs) {
        this(
            () -> {
                throw new UnsupportedOperationException("Caching unsupported");
            },
            () -> false,
            ojs
        );
    }

    /**
     * Primary constructor.
     * @param map Objectionaries hash-map.
     * @param cache Cache path.
     * @param cached Use cache.
     */
    private ObjsDefault(
        final Scalar<Path> cache,
        final Scalar<Boolean> cached,
        final Map<? super String, Objectionary> map
    ) {
        this.cache = new Unchecked<>(cache);
        this.cached = cached;
        this.map = map;
    }

    @Override
    public Objectionaries with(final CommitHash hash, final Objectionary objectionary) {
        this.map.putIfAbsent(hash.value(), objectionary);
        return this;
    }

    @Override
    public Input object(final CommitHash hash, final String name) throws IOException {
        return this.objectionary(hash).get(name);
    }

    @Override
    public boolean contains(final CommitHash hash, final String name) throws IOException {
        return this.objectionary(hash).contains(name);
    }

    private Objectionary objectionary(final CommitHash hash) {
        final CommitHash sticky = new ChCached(hash);
        if (!this.map.containsKey(sticky.value())) {
            final CommitHash narrow = new ChNarrow(sticky);
            this.with(
                sticky,
                new OyFallbackSwap(
                    new OyHome(
                        narrow,
                        this.cache.value()
                    ),
                    new OyCaching(
                        narrow,
                        this.cache.value(),
                        new OyIndexed(
                            new OyRemote(sticky)
                        )
                    ),
                    this.cached
                )
            );
        }
        return this.map.get(sticky.value());
    }
}
