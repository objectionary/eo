/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * Asserting that path contains a files matching provided globs.
 * @since 0.31.0
 * @todo #4777:30min Remove {@link ContainsFiles} duplicate.
 *  We have exactly the same class in eo-maven-plugin module.
 *  We need to keep only one copy of this class and use it in both
 *  eo-maven-plugin and eo-integration-tests modules.
 *  Don't forget to remove exclusion from 'simian.yaml' as well.
 */
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleCorrectTestName",
    "JTCOP.RuleInheritanceInTests"
})
final class ContainsFiles extends TypeSafeMatcher<Path> {
    /**
     * Patterns.
     */
    private final String[] globs;

    /**
     * Ctor.
     * @param glbs Patterns
     */
    ContainsFiles(final String... glbs) {
        this.globs = Arrays.copyOf(glbs, glbs.length);
    }

    @Override
    public void describeTo(final Description description) {
        description.appendText(String.format("Matching globs: %s", Arrays.toString(this.globs)));
    }

    @Override
    public boolean matchesSafely(final Path path) {
        return Arrays.stream(this.globs)
            .anyMatch(
                glob -> ContainsFiles.matchesGlob(
                    path,
                    glob
                )
            );
    }

    /**
     * Returns whether a path matches a file pattern.
     * @param item The path.
     * @param glob The file pattern.
     * @return True if the item matches the glob.
     */
    private static boolean matchesGlob(final Path item, final String glob) {
        try {
            return Files.walk(item)
                .anyMatch(
                    FileSystems
                        .getDefault()
                        .getPathMatcher(
                            String.format(
                                "glob:%s",
                                glob
                            )
                        )::matches
                );
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Error while matching glob=`%s` for %s",
                    glob,
                    item
                ),
                ex
            );
        }
    }
}
