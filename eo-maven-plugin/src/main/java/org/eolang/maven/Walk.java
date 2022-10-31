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
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.list.ListEnvelope;

/**
 * Walk through files in a directory.
 *
 * @since 0.1
 */
final class Walk extends ListEnvelope<Path> {

    /**
     * The home.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory
     * @throws IOException If fails
     */
    Walk(final Path dir) throws IOException {
        this(dir, Walk.list(dir));
    }

    /**
     * Ctor.
     *
     * @param dir The directory
     * @param list The list
     */
    private Walk(final Path dir, final List<Path> list) {
        super(list);
        this.home = dir;
    }

    /**
     * Includes this globs.
     * @param globs List of them
     * @return New Walk
     */
    Walk includes(final Collection<String> globs) {
        return new Walk(
            this.home,
            this.stream()
                .filter(
                    file -> Walk.stream(globs).anyMatch(
                        glob -> this.matches(glob, file)
                    )
                )
                .collect(Collectors.toList())
        );
    }

    /**
     * Includes this globs.
     * @param globs List of them
     * @return New Walk
     */
    Walk excludes(final Collection<String> globs) {
        return new Walk(
            this.home,
            this.stream()
                .filter(
                    file -> Walk.stream(globs).noneMatch(
                        glob -> this.matches(glob, file)
                    )
                )
                .collect(Collectors.toList())
        );
    }

    /**
     * Get stream.
     * @param globs The globs
     * @return Stream
     */
    private static Stream<String> stream(final Collection<String> globs) {
        return globs.stream();
    }

    /**
     * List them all.
     * @param dir The dir
     * @return List
     * @throws IOException If fails
     */
    private static List<Path> list(final Path dir) throws IOException {
        final List<Path> files = new LinkedList<>();
        if (Files.exists(dir)) {
            files.addAll(
                Files.walk(dir)
                    .filter(file -> !file.toFile().isDirectory())
                    .collect(Collectors.toList())
            );
        }
        return files;
    }

    /**
     * Create glob matcher from text.
     * @param text The pattern, e.g. "**&#47;*.java"
     * @param file The file to match
     * @return Matcher
     */
    private boolean matches(final String text, final Path file) {
        final Path rel = Paths.get(
            file.toAbsolutePath().toString().substring(
                this.home.toAbsolutePath().toString().length() + 1
            )
        );
        return FileSystems.getDefault().getPathMatcher(
            String.format("glob:%s", text)
        ).matches(rel);
    }

}
