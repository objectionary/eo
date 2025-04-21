/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.Text;
import org.cactoos.text.TextOf;

/**
 * Filesystem abstraction.
 * The purpose of this interface is to reduce the coupling between the application
 * logic and the filesystem.
 * This allows for easier testing and mocking of the filesystem operations.
 * For example, you can create a {@link Fake} implementation of this interface for unit tests
 * and use {@link Real} implementation in production code.
 *
 * @since 0.57
 */
interface Filesystem {

    /**
     * Reads a file from the filesystem.
     * @param path Path to the file
     * @return Text content of the file
     * @throws IOException If an I/O error occurs
     */
    Text read(Path path) throws IOException;

    /**
     * Saves content to a file in the filesystem.
     * @param path Path to the file
     * @param content Content to save
     * @return Path to the saved file
     * @throws IOException If an I/O error occurs
     */
    Path save(Path path, Text content) throws IOException;

    /**
     * Real implementation of the Filesystem interface.
     * This implementation uses the actual filesystem to read and write files.
     * @since 0.57
     */
    final class Real implements Filesystem {
        @Override
        public Text read(final Path path) throws IOException {
            return new TextOf(path);
        }

        @Override
        public Path save(final Path path, final Text content) throws IOException {
            return new Saved(content, path).value();
        }
    }

    /**
     * Fake implementation of the Filesystem interface.
     * This implementation is used for testing purposes.
     * @since 0.57
     */
    final class Fake implements Filesystem {

        /**
         * In-memory storage for file content.
         */
        private final Map<? super Path, Text> storage;

        /**
         * Constructor.
         */
        Fake() {
            this(new ConcurrentHashMap<>(0));
        }

        /**
         * Constructor.
         * @param storage In-memory storage for file content
         */
        Fake(final Map<? super Path, Text> storage) {
            this.storage = storage;
        }

        @Override
        public Text read(final Path path) throws IOException {
            return this.storage.get(path);
        }

        @Override
        public Path save(final Path path, final Text content) throws IOException {
            this.storage.put(path, content);
            return path;
        }
    }
}
