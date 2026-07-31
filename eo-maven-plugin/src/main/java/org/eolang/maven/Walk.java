/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.PatternSyntaxException;
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
        final Collection<PathMatcher> matchers = Walk.compiled(globs);
        return new Walk(
            this.home,
            this.stream().filter(
                file -> matchers.stream().anyMatch(
                    matcher -> matcher.matches(this.relative(file))
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
        final Collection<PathMatcher> matchers = Walk.compiled(globs);
        return new Walk(
            this.home,
            this.stream().filter(
                file -> matchers.stream().noneMatch(
                    matcher -> matcher.matches(this.relative(file))
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
     * Compile every glob once, up front.
     *
     * <p>Compiling here rather than inside the filter matters twice over.
     * It turns one compilation per glob per file into one per glob, and it
     * moves the failure of a glob that cannot be compiled out of the walk
     * and onto the configuration that named it, so the report can say
     * which pattern is at fault and where it came from instead of
     * arriving as a regex error from inside a file scan.</p>
     *
     * @param globs The patterns, e.g. "**&#47;*.java"
     * @return Matchers, in the order given
     */
    private static Collection<PathMatcher> compiled(final Collection<String> globs) {
        return globs.stream().map(Walk::matcher).collect(Collectors.toList());
    }

    /**
     * Compile one glob.
     * @param glob The pattern
     * @return Matcher
     */
    private static PathMatcher matcher(final String glob) {
        try {
            return FileSystems.getDefault().getPathMatcher(String.format("glob:%s", glob));
        } catch (final PatternSyntaxException ex) {
            throw new IllegalArgumentException(
                String.format(
                    "The glob '%s', configured to select sources, is not a valid glob pattern",
                    glob
                ),
                ex
            );
        }
    }

    /**
     * The file's path relative to the directory being walked.
     * @param file The file
     * @return Relative path
     */
    private Path relative(final Path file) {
        return Paths.get(
            file.toAbsolutePath().toString().substring(
                this.home.toAbsolutePath().toString().length() + 1
            )
        );
    }
}
