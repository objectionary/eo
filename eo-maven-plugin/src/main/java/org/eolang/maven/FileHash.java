/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.Md5DigestOf;
import org.cactoos.bytes.UncheckedBytes;
import org.cactoos.io.InputOf;

/**
 * MD5 hash of a file (its content).
 *
 * @since 0.24
 */
final class FileHash {

    /**
     * The file.
     */
    private final Path file;

    /**
     * Ctor.
     * @param path The name of the file
     */
    FileHash(final Path path) {
        this.file = path;
    }

    @Override
    public String toString() {
        final String hash;
        if (Files.exists(this.file)) {
            hash = Arrays.toString(
                new UncheckedBytes(
                    new Md5DigestOf(new InputOf(new BytesOf(this.file)))
                ).asBytes()
            );
        } else {
            hash = "";
        }
        return hash;
    }
}
