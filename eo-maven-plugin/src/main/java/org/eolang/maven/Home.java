/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import org.cactoos.Bytes;
import org.cactoos.Input;
import org.cactoos.Text;

/**
 * Location for the files.
 * @since 0.32.0
 */
interface Home {
    /**
     * Saving string.
     *
     * @param str String
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    void save(String str, Path path) throws IOException;

    /**
     * Saving text.
     *
     * @param txt Text
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    void save(Text txt, Path path) throws IOException;

    /**
     * Saving stream.
     *
     * @param stream Input stream
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    void save(InputStream stream, Path path) throws IOException;

    /**
     * Saving bytes.
     *
     * @param bytes Byte array
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    void save(byte[] bytes, Path path) throws IOException;

    /**
     * Saving input.
     *
     * @param input Input
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     * @throws IllegalArgumentException If given path is absolute
     */
    void save(Input input, Path path) throws IOException;

    /**
     * Check if exists.
     *
     * @param path Cwd-relative path to file
     * @return True if exists
     * @throws IllegalArgumentException If given path is absolute
     */
    boolean exists(Path path);

    /**
     * Load bytes from file by path.
     *
     * @param path Cwd-relative path to file
     * @return Bytes of file
     * @throws IOException if method can't find the file by path or
     *  if some exception happens during reading the file
     * @throws IllegalArgumentException If given path is absolute
     */
    Bytes load(Path path) throws IOException;

    /**
     * Absolute path to a file.
     *
     * @param path Cwd-relative path to file
     * @return Absolute path
     */
    Path absolute(Path path);

    /**
     * Verifies that given path is relative and throws exception.
     * @param path Path to be verified
     * @return Given path if it's relative
     * @throws IllegalArgumentException If given path is Absolute
     */
    Path onlyRelative(Path path);
}
