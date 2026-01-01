/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
import java.nio.file.Path;
import java.util.regex.Pattern;

/**
 * Make program name from a path.
 *
 * @since 0.1
 */
final class Unplace {
    /**
     * Pattern to catch .eo files.
     */
    private static final Pattern EO = Pattern.compile(".eo$");

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
        return Unplace.EO.matcher(
            file.toString().substring(this.parent.toString().length() + 1)
        ).replaceAll("").replace(File.separator, ".");
    }
}
