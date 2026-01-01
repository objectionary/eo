/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.BiFunc;

/**
 * Footprint is a function that accepts path to source and
 * target files, updates target file and returns it.
 * @since 0.41.0
 */
interface Footprint extends BiFunc<Path, Path, Path> {
    @Override
    Path apply(Path source, Path target) throws IOException;
}
