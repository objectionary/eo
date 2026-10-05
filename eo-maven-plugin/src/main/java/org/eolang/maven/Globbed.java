/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.util.regex.PatternSyntaxException;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * A glob pattern that decides whether a walk takes a file.
 *
 * <p>The pattern arrives from a parameter in pom.xml, is compiled on the
 * first file it is asked about, and the compiled matcher is kept for the
 * files that follow. A pattern that cannot be compiled is reported with
 * its own text and with the role it plays, because the error the JDK
 * raises quotes the regular expression the glob was translated into and
 * names neither the glob nor what it was meant to select.</p>
 *
 * <p>The name is not {@code Glob} on purpose. Plexus resolves the name of
 * a configuration element to a class in this package, and {@code <glob>}
 * is what {@code <keepBinaries>} in {@code eo-runtime/pom.xml} calls its
 * members, so a class named {@code Glob} is taken for the type of those
 * members and the build dies on its missing no-argument constructor.</p>
 *
 * @since 0.73.4
 */
final class Globbed {

    /**
     * The pattern, as pom.xml writes it.
     */
    private final String text;

    /**
     * What this glob does to the files it matches.
     */
    private final String role;

    /**
     * The matcher, compiled once.
     */
    private final Unchecked<PathMatcher> matcher;

    /**
     * Ctor.
     *
     * @param pattern The glob pattern
     * @param does What the glob does to the files it matches
     */
    Globbed(final String pattern, final String does) {
        this.text = pattern;
        this.role = does;
        this.matcher = new Unchecked<>(new Sticky<>(this::compiled));
    }

    /**
     * Does this glob match the file?
     *
     * @param file The file, relative to the home of the walk
     * @return TRUE if it matches
     */
    boolean matches(final Path file) {
        return this.matcher.value().matches(file);
    }

    private PathMatcher compiled() {
        try {
            return FileSystems.getDefault().getPathMatcher(
                String.format("glob:%s", this.text)
            );
        } catch (final PatternSyntaxException ex) {
            throw new IllegalArgumentException(
                String.format(
                    "The glob '%s', which %s, is not a valid pattern",
                    this.text,
                    this.role
                ),
                ex
            );
        }
    }
}
