/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
package org.eolang.maven.hash;

import java.nio.file.Path;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.scalar.FirstOf;
import org.cactoos.scalar.Mapped;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.Flattened;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;

/**
 * Commit Hash from text.
 *
 * @since 0.28.11
 */
public final class ChText implements CommitHash {

    /**
     * Commit Hash text source.
     */
    private final Scalar<String> source;

    /**
     * Tag.
     */
    private final String tag;

    /**
     * Production constructor.
     *
     * @param file Path to offline file with hashes and tags.
     * @param tag Lookup tag.
     */
    public ChText(final Path file, final String tag) {
        this(() -> new TextOf(new InputOf(file)).asString(), tag);
    }

    /**
     * The main constructor.
     *
     * @param source Text source.
     * @param tag Lookup tag.
     */
    ChText(final Scalar<String> source, final String tag) {
        this.source = source;
        this.tag = tag;
    }

    @Override
    public String value() {
        final String hash;
        if (this.tag.matches("[0-9a-fA-F]{40}")) {
            hash = this.tag;
        } else {
            hash = this.inside();
        }
        return hash;
    }

    /**
     * Find and return Git SHA for the given tag (or throw if not found).
     * @return The Git SHA
     */
    private String inside() {
        return new Unchecked<>(
            new Mapped<>(
                Text::asString,
                new FirstOf<>(
                    new Split(
                        new Flattened(
                            new FirstOf<>(
                                new Filtered<>(
                                    t -> t.asString().matches(
                                        String.format("^.+\\s\\Q%s\\E$", this.tag)
                                    ),
                                    new Split(new TextOf(this.source), "\n")
                                ),
                                () -> {
                                    throw new NotFound(
                                        String.format(
                                            "Git SHA not found for the '%s' tag, probably it doesn't exist in this file: https://github.com/objectionary/home/blob/gh-pages/tags.txt",
                                            this.tag
                                        )
                                    );
                                }
                            )
                        ),
                        "\\s+"
                    ),
                    () -> {
                        throw new NotFound(
                            String.format(
                                "The tag '%s' was found, but there is no corresponding Git SHA for it in the line, most probably something is wrong in this file: https://github.com/objectionary/home/blob/gh-pages/tags.txt",
                                this.tag
                            )
                        );
                    }
                )
            )
        ).value();
    }

    /**
     * The exception for case when hash not found.
     *
     * @since 0.28.11
     */
    static final class NotFound extends RuntimeException {
        /**
         * The main constructor.
         * @param cause The cause of it
         */
        private NotFound(final String cause) {
            super(cause);
        }
    }
}
