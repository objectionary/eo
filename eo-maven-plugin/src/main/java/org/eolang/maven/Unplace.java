/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
import java.nio.file.Path;

/**
 * Make program name from a path.
 *
 * @since 0.1
 */
final class Unplace {

    /**
     * The parent dir.
     */
    private final Path parent;

    /**
     * Ctor.
     * @param dir The name of the parent dir
     */
    Unplace(final File dir) {
        this(dir.toPath());
    }

    /**
     * Ctor.
     * @param dir The name of the parent dir
     */
    Unplace(final Path dir) {
        this.parent = dir;
    }

    /**
     * Make a program name.
     * @param file The file
     * @return The name of the program
     */
    String make(final Path file) {
        return file.toString().substring(
            this.parent.toString().length() + 1
        ).replaceAll(".eo$", "").replace(File.separator, ".");
    }
}
