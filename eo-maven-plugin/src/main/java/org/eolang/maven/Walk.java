/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.list.ListEnvelope;

/**
 * Walk through files in a directory.
 * @since 0.1
 */
final class Walk extends ListEnvelope<Path> {

    /**
     * The home.
     */
    private final Path home;

    /**
     * Ctor.
     * @param dir The directory
     */
    Walk(final Path dir) {
        this(dir, Walk.list(dir));
    }

    /**
     * Ctor.
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
        final List<PathMatcher> matchers = new GlobPatterns(globs).value();
        return this.filtered(
            path -> matchers.stream().anyMatch(
                matcher -> this.matches(matcher, path)
            )
        );
    }

    /**
     * Includes this globs.
     * @param globs List of them
     * @return New Walk
     */
    @SuppressWarnings("PMD.LooseCoupling")
    Walk excludes(final Collection<String> globs) {
        final List<PathMatcher> matchers = new GlobPatterns(globs).value();
        return this.filtered(
            path -> matchers.stream().noneMatch(
                matcher -> this.matches(matcher, path)
            )
        );
    }

    /**
     * Filtered walk.
     * @param path Path predicate
     * @return Filtered walk
     */
    @SuppressWarnings("PMD.LooseCoupling")
    private Walk filtered(final Predicate<Path> path) {
        return new Walk(
            this.home,
            this.stream().filter(path).collect(Collectors.toList())
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
     * @param dir The dir
     * @return Collection of files
     * @throws IOException If fails.
     */
    private static Collection<Path> regular(final Path dir) throws IOException {
        try (Stream<Path> walk = Files.walk(dir)) {
            return walk.filter(file -> !file.toFile().isDirectory())
                .collect(Collectors.toList());
        }
    }

    /**
     * Does matcher match the path?
     * @param matcher Matcher
     * @param file The path to match
     * @return Matcher
     */
    private boolean matches(final PathMatcher matcher, final Path file) {
        return matcher.matches(
            Paths.get(
                file.toAbsolutePath().toString().substring(
                    this.home.toAbsolutePath().toString().length() + 1
                )
            )
        );
    }
}
