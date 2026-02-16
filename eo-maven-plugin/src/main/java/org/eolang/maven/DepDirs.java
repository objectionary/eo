/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
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
 * Walk through directories in the provided directory and find names of
 * Maven dependencies.
 *
 * <p>Each dependency is a subdirectory of the <b>forth</b> level,
 * for example {@code "org.eolang/eo-runtime/jar-with-dependencies/0.10.0"},
 * where the first part is the {@code groupId}, the second is the
 * {@code artifactId}, the third is the {@code classifier}, and the forth
 * is the {@code version}.</p>
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
    @SuppressWarnings("PMD.UnnecessaryLocalBeforeReturn")
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
