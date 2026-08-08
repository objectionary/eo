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
import org.cactoos.list.ListEnvelope;

/**
 * Globs, compiled to {@link java.util.regex.Pattern}.
 * @since 0.62.3
 */
final class GlobPatterns extends ListEnvelope<PathMatcher> {

    /**
     * Ctor.
     * @param globs Globs
     */
    GlobPatterns(final Collection<String> globs) {
        this(
            globs.stream().map(
                glob -> FileSystems.getDefault().getPathMatcher(String.format("glob:%s", glob))
            ).collect(Collectors.toList())
        );
    }

    /**
     * Ctor.
     * @param matchers Matchers
     */
    GlobPatterns(final List<PathMatcher> matchers) {
        super(matchers);
    }
}
