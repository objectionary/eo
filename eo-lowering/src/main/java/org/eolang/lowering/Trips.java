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
import org.cactoos.Text;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.cactoos.text.Trimmed;
import org.cactoos.text.UncheckedText;

/**
 * How many trips over the wire the runs of one document made.
 *
 * <p>It takes the path of a file. The engine, which lives in a process of
 * its own, appends the count of its run; the build sums the lines,
 * answers the total, and clears the file before a document goes out again.
 * This is the only way the build can say what a document cost.</p>
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
     */
    public long total() {
        long out = 0L;
        if (Files.exists(this.file)) {
            for (final Text line : new Split(new TextOf(this.file), "\\R")) {
                final String count = new UncheckedText(new Trimmed(line)).asString();
                if (!count.isEmpty()) {
                    out += Long.parseLong(count);
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
