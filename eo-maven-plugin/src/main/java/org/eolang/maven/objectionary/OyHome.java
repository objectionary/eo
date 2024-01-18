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

import java.io.FileNotFoundException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.eolang.maven.Place;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.util.Rel;

/**
 * Objectionary stored locally.
 *
 * @since 1.0
 * @checkstyle IllegalCatchCheck (150 lines)
 */
public final class OyHome implements Objectionary {
    /**
     * Local storage.
     */
    private final Path home;

    /**
     * Version.
     */
    private final String version;

    /**
     * Ctor.
     * @param hash Commit hash.
     * @param path Root.
     */
    public OyHome(final CommitHash hash, final Path path) {
        this(hash.value(), path);
    }

    /**
     * Ctor.
     * @param ver Version.
     * @param path Root.
     */
    public OyHome(final String ver, final Path path) {
        this.version = ver;
        this.home = path;
    }

    @Override
    public String toString() {
        return String.format(
            "%s (%s)",
            new Rel(this.home), this.version
        );
    }

    @Override
    public Input get(final String name) throws FileNotFoundException {
        final Path file = new Place(name).make(
            this.home
                .resolve("pulled")
                .resolve(this.version),
            "eo"
        );
        if (!Files.exists(file)) {
            throw new FileNotFoundException(name);
        }
        return new InputOf(file);
    }

    @Override
    public boolean contains(final String name) {
        return Files.exists(
            new Place(name).make(
                this.home
                    .resolve("pulled")
                    .resolve(this.version),
                "eo"
            )
        );
    }
}
