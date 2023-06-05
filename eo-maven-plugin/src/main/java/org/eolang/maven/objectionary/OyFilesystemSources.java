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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * Objectionary stored locally in the filesystem.
 * The aim of this class is to download object sources directly from the filesystem, for example,
 * from the project itself. It is useful for testing purposes.
 *
 * @since 0.30
 */
public final class OyFilesystemSources implements Objectionary {

    /**
     * Root where to look for object sources.
     */
    private final Path home;

    /**
     * Constructor.
     */
    public OyFilesystemSources() {
        this(OyFilesystemSources.defaultHome());
    }

    /**
     * Constructor.
     * @param root Root where to look for object sources.
     */
    private OyFilesystemSources(final Path root) {
        this.home = root;
    }

    @Override
    public Input get(final String name) throws IOException {
        return new InputOf(this.object(name));
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return Files.exists(this.object(name));
    }

    /**
     * Path to the object.
     * @param name Object name.
     * @return Path to the object.
     */
    private Path object(final String name) {
        return this.home.resolve(
            String.format(
                "src/main/eo/%s.eo",
                name.replace(".", "/")
            )
        );
    }

    /**
     * Default home.
     * @return Path to the default home.
     */
    private static Path defaultHome() {
        return Paths.get(
            System.getProperty(
                "runtime.path",
                Paths.get("")
                    .toAbsolutePath()
                    .resolve("eo-runtime")
                    .toString()
            )
        );
    }
}
