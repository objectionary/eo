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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.list.ListEnvelope;

/**
 * Default implementation of {@link Walk}.
 *
 * <p>Only regular files are walked. A directory is not one, and neither is
 * a FIFO, a socket, a device node or a link with nothing at the end of it,
 * and a goal handed such an entry cannot make an EO program out of it: it
 * would hash and read it, and reading a FIFO waits for a writer that never
 * comes. A link to an ordinary file is still walked, since
 * {@link Files#isRegularFile(Path, java.nio.file.LinkOption...)} follows
 * links by default, and such a source reads exactly like the file it names.</p>
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
        this(dir, WkDefault.list(dir));
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
        return new WkDefault(
            this.home,
            this.stream().filter(
                file -> globs.stream().anyMatch(
                    glob -> this.matches(glob, file)
                )
            )
            .collect(Collectors.toList())
        );
    }

    @Override
    public Walk excludes(final Collection<String> globs) {
        return new WkDefault(
            this.home,
            this.stream().filter(
                file -> globs.stream().noneMatch(
                    glob -> this.matches(glob, file)
                )
            )
            .collect(Collectors.toList())
        );
    }

    private static List<Path> list(final Path dir) {
        try {
            final List<Path> files = new ArrayList<>(0);
            if (Files.exists(dir)) {
                files.addAll(WkDefault.regular(dir));
            }
            return files;
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Can't read files in %s folder during a walk", dir),
                ex
            );
        }
    }

    private static Collection<Path> regular(final Path dir) throws IOException {
        try (Stream<Path> walk = Files.walk(dir)) {
            return walk.filter(Files::isRegularFile)
                .collect(Collectors.toList());
        }
    }

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
