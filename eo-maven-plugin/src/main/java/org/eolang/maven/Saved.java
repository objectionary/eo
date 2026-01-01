/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.cactoos.Input;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;

/**
 * Content saved to the file.
 * Returns path to the file
 * @since 0.41.0
 */
final class Saved implements Scalar<Path> {
    /**
     * Absolute path to save content to.
     */
    private final Path target;

    /**
     * Content lambda.
     */
    private final Input content;

    /**
     * Ctor.
     * @param content Content as string
     * @param target Path to save content to
     */
    Saved(final String content, final Path target) {
        this(content.getBytes(StandardCharsets.UTF_8), target);
    }

    /**
     * Ctor.
     * @param content Content as bytes
     * @param target Path to save content to
     */
    Saved(final byte[] content, final Path target) {
        this(new InputOf(content), target);
    }

    /**
     * Ctor.
     * @param content Content as text
     * @param target Path to save content to
     */
    Saved(final Text content, final Path target) {
        this(new InputOf(content), target);
    }

    /**
     * Ctor.
     * @param content Content as lambda
     * @param target Absolute path to save content to
     */
    Saved(final Input content, final Path target) {
        this.content = content;
        this.target = target;
    }

    @Override
    public Path value() throws IOException {
        final long bytes;
        try {
            if (this.target.toFile().getParentFile().mkdirs()) {
                Logger.debug(
                    this, "Directory created: %[file]s",
                    this.target.getParent()
                );
            }
            bytes = new IoChecked<>(
                new LengthOf(
                    new TeeInput(
                        this.content,
                        new OutputTo(this.target)
                    )
                )
            ).value();
            Logger.debug(
                this, "File %s saved (%d bytes)",
                this.target, bytes
            );
        } catch (final IOException ex) {
            throw new IOException(
                String.format(
                    "Failed while trying to save to %s",
                    this.target
                ),
                ex
            );
        }
        return this.target;
    }
}
