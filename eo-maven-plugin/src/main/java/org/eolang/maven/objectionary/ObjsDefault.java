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
package org.eolang.maven.objectionary;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import org.cactoos.Input;
import org.cactoos.Scalar;
import org.cactoos.iterable.Mapped;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.name.ObjectName;

/**
 * Default objectionaries.
 * The class is immutable, but NOT thread-safe.
 * @since 0.29.6
 */
public final class ObjsDefault implements Objectionaries {

    /**
     * Cache path.
     */
    private final Unchecked<Path> cache;

    /**
     * Use cache.
     */
    private final Scalar<Boolean> remote;

    /**
     * Hash-map.
     */
    private final Map<? super String, Objectionary> map;

    /**
     * Constructor.
     * @param cache Cache path.
     * @param remote Use cache.
     */
    public ObjsDefault(final Scalar<Path> cache, final Scalar<Boolean> remote) {
        this(cache, remote, new HashMap<>(0));
    }

    /**
     * Constructor for tests with predefined Objectionaries.
     * @param entries Predefined Objectionaries.
     */
    @SafeVarargs
    public ObjsDefault(final Map.Entry<CommitHash, Objectionary>... entries) {
        this(new Mapped<>(e -> new MapEntry<>(e.getKey().value(), e.getValue()), entries));
    }

    /**
     * Constructor for tests with predefined Objectionaries.
     * @param entries Predefined Objectionaries.
     */
    private ObjsDefault(
        final Iterable<Map.Entry<? extends String, ? extends Objectionary>> entries
    ) {
        this(new MapOf<>(entries));
    }

    /**
     * Constructor for tests.
     * @param ojs Predefined Objectionaries.
     */
    private ObjsDefault(final Map<? super String, Objectionary> ojs) {
        this(ObjsDefault::cacheForTests, () -> false, ojs);
    }

    /**
     * Primary constructor.
     * @param cache Cache path.
     * @param remote Use cache.
     * @param map Objectionaries hash-map.
     */
    private ObjsDefault(
        final Scalar<Path> cache,
        final Scalar<Boolean> remote,
        final Map<? super String, Objectionary> map
    ) {
        this.cache = new Unchecked<>(cache);
        this.remote = remote;
        this.map = map;
    }

    @Override
    public Input object(final ObjectName name) throws IOException {
        return this.objectionary(name.hash()).get(name.value());
    }

    @Override
    public boolean contains(final ObjectName name) throws IOException {
        return this.objectionary(name.hash()).contains(name.value());
    }

    /**
     * Get objectionary by hash.
     * @param hash Commit hash.
     * @return Objectionary.
     */
    private Objectionary objectionary(final CommitHash hash) {
        final CommitHash sticky = new ChCached(hash);
        if (!this.map.containsKey(sticky.value())) {
            final CommitHash narrow = new ChNarrow(sticky);
            this.map.put(
                sticky.value(),
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
                    this.remote
                )
            );
        }
        return this.map.get(sticky.value());
    }

    /**
     * Cache path for tests.
     * @return Cache path.
     */
    private static Path cacheForTests() {
        throw new UnsupportedOperationException(
            String.format(
                "Caching unsupported for tests! If you see this message in runtime it means that you are using wrong constructor of %s",
                ObjsDefault.class
            )
        );
    }
}
