/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Make the place for the object.
 *
 * @since 0.1
 */
final class Place {
    /**
     * Name of the object.
     */
    private final String name;

    /**
     * Ctor.
     * @param obj The name of the object
     */
    Place(final String obj) {
        this.name = obj;
    }

    /**
     * Make a full path.
     * @param ext Extension
     * @return Full path
     */
    Path make(final String ext) {
        return this.make(Paths.get(""), ext);
    }

    /**
     * Make a full path.
     * @param dir The dir
     * @param ext The ext
     * @return Full path
     */
    Path make(final Path dir, final String ext) {
        final StringBuilder out = new StringBuilder();
        out.append(this.name.replace(".", File.separator));
        if (!ext.isEmpty()) {
            out.append('.').append(ext);
        }
        return dir.resolve(out.toString());
    }

}
