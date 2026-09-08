/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.cactoos.list.ListEnvelope;
import org.cactoos.list.ListOf;

/**
 * Default implementation of {@link Walk}.
 *
 * <p>The files arrive from {@link Scanned} and this object narrows them
 * down to the ones a goal asked for. The narrowing stays with
 * {@link Globbed}, in the syntax the JDK reads, because a glob there says
 * things the Ant style of the scan cannot: {@code **.eo} reaches through
 * directories, and {@code EO*$[1-9]*.class} in {@code Unspiling} names a
 * range of digits, which Ant would read as five literal characters.</p>
 *
 * @since 0.1
 */
final class WkDefault extends ListEnvelope<Path> implements Walk {

    /**
     * The home.
     */
    private final Path home;

    /**
     * Ctor.
     * @param dir The directory
     */
    WkDefault(final Path dir) {
        this(dir, new ListOf<>(new Scanned(dir)));
    }

    /**
     * Ctor.
     * @param dir The directory
     * @param list The list
     */
    private WkDefault(final Path dir, final List<Path> list) {
        super(list);
        this.home = dir;
    }

    @Override
    public Walk includes(final Collection<String> globs) {
        final Collection<Globbed> patterns = globs.stream()
            .map(glob -> new Globbed(glob, "includes files into the walk"))
            .collect(Collectors.toList());
        return new WkDefault(
            this.home,
            this.stream().filter(
                file -> patterns.stream().anyMatch(
                    glob -> glob.matches(this.relative(file))
                )
            )
            .collect(Collectors.toList())
        );
    }

    @Override
    public Walk excludes(final Collection<String> globs) {
        final Collection<Globbed> patterns = globs.stream()
            .map(glob -> new Globbed(glob, "excludes files from the walk"))
            .collect(Collectors.toList());
        return new WkDefault(
            this.home,
            this.stream().filter(
                file -> patterns.stream().noneMatch(
                    glob -> glob.matches(this.relative(file))
                )
            )
            .collect(Collectors.toList())
        );
    }

    private Path relative(final Path file) {
        return Paths.get(
            file.toAbsolutePath().toString().substring(
                this.home.toAbsolutePath().toString().length() + 1
            )
        );
    }
}
