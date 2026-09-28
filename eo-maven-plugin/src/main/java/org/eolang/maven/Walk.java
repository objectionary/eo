/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.Collection;
import java.util.List;

/**
 * Walk through files in a directory.
 *
 * @since 0.1
 */
interface Walk extends List<Path> {

    /**
     * Includes this globs.
     *
     * @param globs List of them
     * @return New Walk
     */
    Walk includes(Collection<String> globs);

    /**
     * Excludes this globs.
     *
     * @param globs List of them
     * @return New Walk
     */
    Walk excludes(Collection<String> globs);
}
