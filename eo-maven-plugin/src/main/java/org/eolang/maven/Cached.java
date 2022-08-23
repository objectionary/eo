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

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.text.TextOf;

/**
 * Cached file in EO compilation process. The file is assumed to
 * have text content in any format.
 * @since 1.0
 */
public class Cached {
    /**
     * Name of an object.
     */
    private final String object;

    /**
     * Version tag.
     */
    private final String ver;

    /**
     * Path to cache root.
     */
    private final Path cache;

    /**
     * Ctor.
     * @param ver Version tag
     * @param object Object name
     * @param cache Cache root
     */
    public Cached(final String ver, final String object, final Path cache) {
        this.ver = ver;
        this.object = object;
        this.cache = cache;
    }

    /**
     * Get content of the cached object.
     * @return Content
     * @throws IOException In case of IO issue.
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    public String content() throws IOException {
        try {
            return new TextOf(
                this.path()
            ).asString();
            // @checkstyle IllegalCatchCheck (1 line)
        } catch (final Exception ex) {
            throw new IOException(ex);
        }
    }

    /**
     * Is it cached?
     * @return True if object exists in case and false otherwise
     */
    public boolean exists() {
        return this.path().toFile().exists();
    }

    /**
     * Save content into the cache.
     * @param content Content to cache
     * @throws IOException In case of IO issues
     */
    public void save(final String content) throws IOException {
        new Save(
            content,
            this.path()
        ).save();
    }

    /**
     * Path to the object.
     * @return Absolute path to the object file.
     */
    private Path path() {
        return this.cache.resolve(this.ver).resolve(this.object);
    }
}
