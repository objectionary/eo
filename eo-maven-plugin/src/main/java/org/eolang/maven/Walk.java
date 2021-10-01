/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.maven.plugin.MojoFailureException;
import org.cactoos.list.ListEnvelope;

/**
 * Walk through files in a directory.
 *
 * @since 0.1
 */
final class Walk extends ListEnvelope<Path> {

    /**
     * Ctor.
     *
     * @param dir The directory
     * @throws MojoFailureException If fails
     */
    Walk(final Path dir) throws MojoFailureException {
        super(Walk.list(dir));
    }

    /**
     * List them all.
     * @param dir The dir
     * @return List
     * @throws MojoFailureException If fails
     */
    private static List<Path> list(final Path dir) throws MojoFailureException {
        try {
            final List<Path> files = new LinkedList<>();
            if (Files.exists(dir)) {
                files.addAll(
                    Files.walk(dir)
                        .filter(file -> !file.toFile().isDirectory())
                        .collect(Collectors.toList())
                );
            }
            return files;
        } catch (final IOException ex) {
            throw new MojoFailureException(
                String.format(
                    "Can't list XML files in %s",
                    dir
                ),
                ex
            );
        }
    }

}
