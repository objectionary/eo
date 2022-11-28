/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * Asserting that path contains a file matching provided glob.
 * @since 0.28.12
 */
final class ContainsFile extends TypeSafeMatcher<Path> {

    /**
     * Glob pattern to match against.
     */
    private final String glob;

    /**
     * Ctor.
     * @param glob Pattern
     */
    ContainsFile(final String glob) {
        this.glob = glob;
    }

    @Override
    public void describeTo(final Description description) {
        description.appendText(String.format("Matching glob=`%s`", this.glob));
    }

    @Override
    public boolean matchesSafely(final Path item) {
        try {
            return Files.walk(item)
                .anyMatch(
                    FileSystems.getDefault()
                        .getPathMatcher(
                            String.format(
                                "glob:%s",
                                this.glob
                            )
                        )::matches
                );
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Error while matching glob=`%s` for %s",
                    this.glob,
                    item
                ),
                ex
            );
        }
    }
}
