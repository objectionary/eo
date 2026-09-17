/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * How much a directory of tables holds, in words a log line can carry.
 *
 * <p>A path on its own says where to look and nothing about what is there.
 * Whoever reads the log wants to know whether the tables were written at all
 * and how big they came out, since a table that shrank by half between two
 * builds is the first sign that a rule stopped seeing something.</p>
 *
 * @since 0.71.0
 */
final class Tabled {

    /**
     * The directory the tables are in.
     */
    private final Path dir;

    /**
     * Ctor.
     *
     * @param tables The directory the tables are in
     */
    Tabled(final Path tables) {
        this.dir = tables;
    }

    /**
     * How many tables there are and how much they weigh together.
     *
     * @return The description, for a log line
     * @throws IOException If the directory cannot be read
     */
    String asString() throws IOException {
        final Collection<Path> files = new ArrayList<>(0);
        if (Files.exists(this.dir)) {
            try (Stream<Path> all = Files.list(this.dir)) {
                files.addAll(all.filter(Files::isRegularFile).collect(Collectors.toList()));
            }
        }
        long bytes = 0L;
        for (final Path file : files) {
            bytes = bytes + Files.size(file);
        }
        return Logger.format("%d table(s), %[size]s total", files.size(), bytes);
    }
}
