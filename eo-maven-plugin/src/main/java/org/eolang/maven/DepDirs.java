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
package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.list.ListEnvelope;

/**
 * Walk through directories in a directory and find dep names. Each
 * dep is a subdirectory of the FOURTH level,
 * for example "org.eolang/eo-runtime/jar-with-dependencies/0.10.0".
 *
 * @since 0.13
 */
final class DepDirs extends ListEnvelope<String> {

    /**
     * Ctor.
     *
     * @param dir The directory
     * @throws IOException If fails
     */
    DepDirs(final Path dir) throws IOException {
        super(DepDirs.list(dir));
    }

    /**
     * List them all.
     * @param dir The dir
     * @return List
     * @throws IOException If fails
     */
    private static List<String> list(final Path dir) throws IOException {
        final List<String> names = new LinkedList<>();
        if (Files.exists(dir)) {
            final String home = dir.toAbsolutePath().toString();
            try (Stream<Path> paths = Files.find(dir, 4, (t, u) -> true)) {
                names.addAll(
                    paths
                        .filter(file -> file.toFile().isDirectory())
                        .map(file -> file.toAbsolutePath().toString())
                        .filter(name -> !name.equals(home))
                        .map(name -> name.substring(home.length() + 1))
                        .filter(name -> name.split(Pattern.quote(File.separator)).length == 4)
                        .collect(Collectors.toList())
                );
            }
        }
        return names;
    }

}
