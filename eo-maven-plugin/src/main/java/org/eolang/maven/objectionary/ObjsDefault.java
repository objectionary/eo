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
import java.util.HashMap;
import java.util.Map;
import org.cactoos.Scalar;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.CommitHash;

/**
 * Default objectionaries.
 * @since 0.29.6
 */
public final class ObjsDefault implements Objectionaries {

    private final Path cache;

    private final Scalar<Boolean> usecache;

    /**
     * Hash-map.
     */
    private final Map<String, Objectionary> map;

    /**
     * Ctor.
     */
    public ObjsDefault() {
        this(new HashMap<>());
    }

    /**
     * Ctor.
     * @param ojs Objectionaries hash-map.
     */
    ObjsDefault(final Map<String, Objectionary> ojs) {
        throw new UnsupportedOperationException();
    }

    public ObjsDefault(final Path cache, final Scalar<Boolean> usecache) {
        this(cache, usecache, new HashMap<>());
    }

    /**
     * Ctor.
     * @param map Objectionaries hash-map.
     * @param cache Cache path.
     * @param usecache Use cache.
     */
    public ObjsDefault(
        final Path cache,
        final Scalar<Boolean> usecache,
        final Map<String, Objectionary> map
    ) {
        this.cache = cache;
        this.usecache = usecache;
        this.map = map;
    }

    @Override
    public Objectionaries with(final CommitHash hash, final Objectionary objectionary) {
        this.map.putIfAbsent(hash.value(), objectionary);
        return this;
    }

    @Override
    public Objectionary get(final CommitHash hash) {
        return this.map.get(hash.value());
    }

    @Override
    public boolean contains(final CommitHash hash, final String name) {
        try {
            return this.byHash(hash).contains(name);
        } catch (final IOException ex) {
            //todo
            throw new IllegalStateException(ex);
        }
    }

    private Objectionary byHash(final CommitHash hash) {
        final CommitHash cached = new ChCached(hash);
        final CommitHash narrow = new ChNarrow(cached);
        return this.with(
            cached,
            new OyFallbackSwap(
                new OyHome(
                    narrow,
                    this.cache
                ),
                new OyCaching(
                    narrow,
                    this.cache,
                    new OyIndexed(
                        new OyRemote(cached)
                    )
                ),
                this.usecache
            )
        ).get(cached);
    }
}
