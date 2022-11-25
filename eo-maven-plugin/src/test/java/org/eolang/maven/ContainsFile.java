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
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * Asserting that path contains a file matching provided glob.
 * @since 0.28.12
 * @checkstyle ProtectedMethodInFinalClassCheck (30 lines)
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
    protected boolean matchesSafely(final Path item) {
        final AtomicBoolean matched = new AtomicBoolean(false);
        try {
            Files.walkFileTree(
                item,
                new ActionOnMatch(path -> matched.set(true), this.glob)
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
        return matched.get();
    }

    /**
     * Preform supplied actions on match.
     *
     * @since 0.28.12
     */
    private static class ActionOnMatch extends SimpleFileVisitor<Path> {
        /**
         * Action to perform.
         */
        private final Consumer<Path> action;

        /**
         * Glob match criteria.
         */
        private final String glob;

        /**
         * Ctor.
         * @param action Action to perform.
         * @param glob Glob to match.
         */
        ActionOnMatch(final Consumer<Path> action, final String glob) {
            this.action = action;
            this.glob = glob;
        }

        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) {
            final FileVisitResult result;
            if (FileSystems.getDefault()
                .getPathMatcher(
                    String.format(
                        "glob:%s",
                        this.glob
                    )
                ).matches(file)
            ) {
                this.action.accept(file);
                result = FileVisitResult.TERMINATE;
            } else {
                result = FileVisitResult.CONTINUE;
            }
            return result;
        }
    }
}
