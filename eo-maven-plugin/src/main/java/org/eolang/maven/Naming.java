/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.FileSystem;

/**
 * The step that proves the file system can name a file after an EO object.
 *
 * @since 0.74
 */
final class Naming implements Step {

    /**
     * The file system to name files in.
     */
    private final FileSystem system;

    /**
     * The name of an EO object to try.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param fsys The file system to name files in
     * @param obj The name of an EO object to try
     */
    Naming(final FileSystem fsys, final String obj) {
        this.system = fsys;
        this.name = obj;
    }

    @Override
    public void exec() {
        try {
            this.system.getPath(this.name);
        } catch (final IllegalArgumentException ex) {
            throw new IllegalStateException(
                String.format(
                    "The file system cannot name a file \"%s\" after an EO object, run the build with a UTF-8 locale such as LANG=C.UTF-8",
                    this.name
                ),
                ex
            );
        }
    }
}
