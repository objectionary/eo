/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * How many trips over the wire the runs of one document made.
 *
 * <p>The engine lives in a process of its own, started by phino once per
 * run, so the build cannot read the count out of its memory: the engine
 * appends the trips of its run as one line of the file the launcher names
 * in its environment, and the build sums the lines of every run the
 * document took. A document is counted from scratch on every build, since
 * the file of the last one is dropped before the first fragment goes
 * out.</p>
 *
 * @since 0.77.0
 */
public final class Trips {

    /**
     * The file.
     */
    private final Path file;

    /**
     * Ctor.
     *
     * @param dest The file
     */
    public Trips(final Path dest) {
        this.file = dest;
    }

    /**
     * Append the trips of one run.
     *
     * @param count How many trips the run made
     * @throws IOException If the file cannot be written
     */
    public void record(final long count) throws IOException {
        Files.createDirectories(this.file.toAbsolutePath().getParent());
        Files.write(
            this.file,
            String.format("%d%n", count).getBytes(StandardCharsets.UTF_8),
            StandardOpenOption.CREATE, StandardOpenOption.APPEND
        );
    }

    /**
     * The trips of every run together.
     *
     * @return How many trips the document made
     * @throws IOException If the file cannot be read
     */
    public long total() throws IOException {
        long out = 0L;
        if (Files.exists(this.file)) {
            for (final String line : Files.readAllLines(this.file, StandardCharsets.UTF_8)) {
                if (!line.isEmpty()) {
                    out += Long.parseLong(line.trim());
                }
            }
        }
        return out;
    }

    /**
     * Forget what was recorded.
     *
     * @throws IOException If the file cannot be deleted
     */
    public void reset() throws IOException {
        Files.deleteIfExists(this.file);
    }
}
