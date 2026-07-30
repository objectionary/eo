/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.concurrent.TimeUnit;
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
 *
 * <p>The content is streamed into a sibling temporary file first, then moved
 * onto {@link #target} with {@link StandardCopyOption#ATOMIC_MOVE}. Streaming
 * straight into {@link #target} would let a concurrent reader (or another
 * writer racing on the same path) observe it truncated, mid-write (#5873):
 * an atomic rename means a reader always sees either the previous complete
 * file or the new one, never a partial one.</p>
 *
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
            final Path tmp = Files.createTempFile(
                this.target.getParent(),
                this.target.getFileName().toString(),
                ".tmp"
            );
            try {
                bytes = new IoChecked<>(
                    new LengthOf(
                        new TeeInput(
                            this.content,
                            new OutputTo(tmp)
                        )
                    )
                ).value();
                Saved.moved(tmp, this.target);
            } finally {
                Files.deleteIfExists(tmp);
            }
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

    /**
     * Move a temp file onto its target, atomically where the filesystem
     * supports it, falling back to a plain (still complete-file, just not
     * guaranteed atomic) move on filesystems that don't.
     *
     * <p>Retried on failure: on Windows, {@code REPLACE_EXISTING} throws
     * {@link java.nio.file.AccessDeniedException} instead of replacing the
     * target when a concurrent reader has it open at that instant, unlike
     * POSIX which allows it. The window is brief, so a short retry clears
     * it instead of failing the whole write.</p>
     *
     * @param tmp Temp file to move
     * @param target Destination path
     * @throws IOException If the move fails
     */
    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static void moved(final Path tmp, final Path target) throws IOException {
        try {
            Files.move(
                tmp, target,
                StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING
            );
        } catch (final AtomicMoveNotSupportedException ignored) {
            Files.move(tmp, target, StandardCopyOption.REPLACE_EXISTING);
        }
    }
}
