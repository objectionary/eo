/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
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
     */
    Walk(final Path dir) {
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
    @SuppressWarnings("PMD.LooseCoupling")
    Walk includes(final Collection<String> globs) {
        return new Walk(
            this.home,
            this.stream()
                .filter(
                    file -> globs.stream().anyMatch(
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
    @SuppressWarnings("PMD.LooseCoupling")
    Walk excludes(final Collection<String> globs) {
        return new Walk(
            this.home,
            this.stream()
                .filter(
                    file -> globs.stream().noneMatch(
                        glob -> this.matches(glob, file)
                    )
                )
                .collect(Collectors.toList())
        );
    }

    /**
     * List them all.
     * @param dir The dir
     * @return List
     */
    private static List<Path> list(final Path dir) {
        try {
            final List<Path> files = new LinkedList<>();
            if (Files.exists(dir)) {
                files.addAll(Walk.regular(dir));
            }
            return files;
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Can't read files in %s folder during a walk", dir),
                ex
            );
        }
    }

    /**
     * Get regular files from directory.
     * @param dir The dir.
     * @return Collection of files.
     * @throws IOException If fails.
     */
    private static Collection<Path> regular(final Path dir) throws IOException {
        try (Stream<Path> walk = Files.walk(dir)) {
            return walk.filter(file -> !file.toFile().isDirectory())
                .collect(Collectors.toList());
        }
    }

    /**
     * Create glob matcher from text.
     * @param text The pattern, e.g. "**&#47;*.java"
     * @param file The file to match
     * @return Matcher
     */
    private boolean matches(final String text, final Path file) {
        return FileSystems.getDefault().getPathMatcher(String.format("glob:%s", text)).matches(
            Paths.get(
                file.toAbsolutePath().toString().substring(
                    this.home.toAbsolutePath().toString().length() + 1
                )
            )
        );
    }
}
