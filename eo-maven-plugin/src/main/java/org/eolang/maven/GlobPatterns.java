/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.FileSystems;
import java.nio.file.PathMatcher;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.cactoos.Scalar;

/**
 * Globs, compiled to {@link java.util.regex.Pattern}.
 * @since 0.62.3
 */
final class GlobPatterns implements Scalar<List<PathMatcher>> {

    /**
     * Globs.
     */
    private final Collection<String> globs;

    /**
     * Ctor.
     * @param glbs Globs
     */
    GlobPatterns(final Collection<String> glbs) {
        this.globs = glbs;
    }

    @Override
    public List<PathMatcher> value() {
        return this.globs.stream()
            .map(glob -> FileSystems.getDefault().getPathMatcher(String.format("glob:%s", glob)))
            .collect(Collectors.toList());
    }
}
