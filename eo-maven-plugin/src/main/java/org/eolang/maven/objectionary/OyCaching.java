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
import org.cactoos.Input;
import org.cactoos.io.TeeInput;
import org.eolang.maven.Place;
import org.eolang.maven.hash.CommitHash;

/**
 * Objectionary which caches objects locally.
 *
 * @since 1.0
 */
public final class OyCaching implements Objectionary {

    /**
     * Cache location.
     */
    private final Path cache;

    /**
     * Version or hash.
     */
    private final String version;

    /**
     * Remote Objectionary.
     */
    private final Objectionary primary;

    /**
     * Ctor.
     * @param hash Commit hash.
     * @param cache Cache directory.
     * @param primary Primary objectionary.
     */
    public OyCaching(
        final CommitHash hash,
        final Path cache,
        final Objectionary primary
    ) {
        this(hash.value(), cache, primary);
    }

    /**
     * Ctor.
     * @param ver Version.
     * @param cache Cache directory.
     * @param primary Primary objectionary.
     */
    public OyCaching(
        final String ver,
        final Path cache,
        final Objectionary primary
    ) {
        this.version = ver;
        this.cache = cache;
        this.primary = primary;
    }

    @Override
    public String toString() {
        return String.format(
            "[%s]+(%s cache to %s)",
            this.primary, this.version, this.cache
        );
    }

    @Override
    public Input get(final String name) throws IOException {
        return new TeeInput(
            this.primary.get(name),
            new Place(name).make(
                this.cache
                    .resolve("pulled")
                    .resolve(this.version),
                "eo"
            )
        );
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return this.primary.contains(name);
    }
}
