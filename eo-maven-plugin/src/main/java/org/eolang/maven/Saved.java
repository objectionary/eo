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
import java.util.Objects;
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
 * Content saved to the file; returns path to the file.
 *
 * <p>The content is streamed into a sibling temporary file first, then moved
 * onto {@link #target} with {@link StandardCopyOption#ATOMIC_MOVE}. Streaming
 * straight into {@link #target} would let a concurrent reader (or another
 * writer racing on the same path) observe it truncated, mid-write (#5873):
 * an atomic rename means a reader always sees either the previous complete
 * file or the new one, never a partial one.</p>
 *
 * <p>On Windows that rename fails with
 * {@link java.nio.file.AccessDeniedException} while somebody else keeps a
 * handle on either of the two names: a concurrent reader of {@link #target},
 * another writer racing on the same path, or an anti-virus scanning the
 * freshly written temporary file. All of them let go in a moment, so the
 * only cure is to try again. Ten attempts spread over nine seconds
 * (200ms, 400ms, ... 1800ms) leave enough room for that, while the first
 * retries stay short enough to keep a normal save fast. Randomization stays
 * off on purpose: jcabi randomizes as {@code rand(0, 2^(attempt+1)) * delay},
 * which at the tenth attempt would sleep for minutes.</p>
 *
 * @since 0.41.0
 */
final class Saved implements Scalar<Path> {

    /**
     * Path to save content to, absolute or relative to the current
     * working directory.
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
     * @param target Path to save content to
     */
    Saved(final Input content, final Path target) {
        this.content = content;
        this.target = target;
    }

    @Override
    public Path value() throws IOException {
        final Path abs = Objects.requireNonNull(this.target, "target").toAbsolutePath();
        final Path dir = Objects.requireNonNull(
            abs.getParent(),
            () -> String.format("%s has no parent directory", abs)
        );
        final long bytes;
        try {
            if (dir.toFile().mkdirs()) {
                Logger.debug(this, "Directory created: %[file]s", dir);
            }
            final Path tmp = Files.createTempFile(
                dir,
                Saved.prefix(this.target),
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
     * The prefix of the temporary file that sits next to the target.
     *
     * <p>{@link Files#createTempFile(Path, String, String, java.nio.file.attribute.FileAttribute[])}
     * refuses a prefix shorter than three characters, while a file name of one or two
     * characters is valid everywhere, so the name is padded here.</p>
     *
     * @param target The file we are going to save
     * @return The prefix, at least three characters long
     */
    private static String prefix(final Path target) {
        final String name = target.getFileName().toString();
        final StringBuilder out = new StringBuilder(name);
        while (out.length() < 3) {
            out.append('_');
        }
        return out.toString();
    }

    @RetryOnFailure(
        attempts = 10, delay = 200L, unit = TimeUnit.MILLISECONDS, randomize = false
    )
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
