/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOfs; // NOPMD

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import org.eolang.ExFailure;

/**
 * File streams.
 * @since 0.40
 */
final class Files {
    /**
     * Files instance.
     */
    static final Files INSTANCE = new Files();

    /**
     * File input streams for reading.
     */
    private final ConcurrentHashMap<String, Object[]> streams;

    /**
     * Ctor.
     */
    private Files() {
        this.streams = new ConcurrentHashMap<>(0);
    }

    /**
     * Open file for reading and writing.
     * @param name Name of the file
     * @throws FileNotFoundException If can't open file
     */
    @SuppressWarnings("java:S2095")
    void open(final String name) throws IOException {
        final Path path = Paths.get(name);
        this.streams.putIfAbsent(
            name,
            new Object[] {
                java.nio.file.Files.newInputStream(path),
                java.nio.file.Files.newOutputStream(path, StandardOpenOption.APPEND),
            }
        );
    }

    /**
     * Read given amount of bytes from file.
     * @param name Name of file
     * @param size Amount of bytes to read
     * @return Read bytes
     * @throws IOException If fails to read
     */
    @SuppressWarnings({"PMD.AssignmentInOperand", "PMD.CloseResource"})
    byte[] read(final String name, final int size) throws IOException {
        synchronized (this.streams) {
            if (!this.streams.containsKey(name)) {
                throw new ExFailure(
                    "File input stream with name %s is absent, can't read",
                    name
                );
            }
            final byte[] read = new byte[size];
            int character;
            int processed = 0;
            final InputStream input = (InputStream) this.streams.get(name)[0];
            while (processed < size && (character = input.read()) != -1) {
                read[processed] = (byte) character;
                ++processed;
            }
            return Arrays.copyOf(read, processed);
        }
    }

    /**
     * Write given byte buffer to file.
     * @param name Name of the file
     * @param buffer Byte buffer to write
     * @throws IOException If fails to write
     */
    void write(final String name, final byte[] buffer) throws IOException {
        synchronized (this.streams) {
            if (!this.streams.containsKey(name)) {
                throw new ExFailure(
                    "File output stream with name %s is absent, can't read",
                    name
                );
            }
            ((OutputStream) this.streams.get(name)[1]).write(buffer);
        }
    }

    /**
     * Close files streams.
     * @param name File name
     * @throws IOException If fails to close the streams
     */
    void close(final String name) throws IOException {
        synchronized (this.streams) {
            if (!this.streams.containsKey(name)) {
                throw new ExFailure(
                    "File streams with name %s is absent, can't close",
                    name
                );
            }
            ((InputStream) this.streams.get(name)[0]).close();
            ((OutputStream) this.streams.get(name)[1]).close();
            this.streams.remove(name);
        }
    }
}
