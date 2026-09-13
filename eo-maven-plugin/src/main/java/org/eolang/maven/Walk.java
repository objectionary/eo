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
 * <p>The patterns are Ant ones, the ones every other Maven plugin reads,
 * where {@code **} stands for a run of directories.</p>
 *
 * @since 0.1
 */
interface Walk extends List<Path> {

    /**
     * Includes this patterns.
     *
     * @param patterns List of them
     * @return New Walk
     */
    Walk includes(Collection<String> patterns);

    /**
     * Excludes this patterns.
     *
     * @param patterns List of them
     * @return New Walk
     */
    Walk excludes(Collection<String> patterns);
}
