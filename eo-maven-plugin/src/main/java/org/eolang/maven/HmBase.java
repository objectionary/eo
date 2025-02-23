/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.Bytes;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.bytes.BytesOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;

/**
 * Base location for files.
 *
 * @since 0.27
 */
@SuppressWarnings("PMD.TooManyMethods")
final class HmBase implements Home {
    /**
     * Current working directory.
     */
    private final Path cwd;

    /**
     * Home with "save" functionality.
     */
    private final Home origin;

    /**
     * Ctor.
     *
     * @param file File
     */
    HmBase(final File file) {
        this(file.toPath());
    }

    /**
     * Ctor.
     *
     * @param pth Path
     */
    HmBase(final Path pth) {
        this.cwd = pth;
        this.origin = new HmSave(
            (input, path) -> {
                final Path target = this.absolute(this.onlyRelative(path));
                if (target.toFile().getParentFile().mkdirs()) {
                    Logger.debug(this, "Directory created: %[file]s", target.getParent());
                }
                try {
                    final long bytes = new IoChecked<>(
                        new LengthOf(
                            new TeeInput(
                                input,
                                new OutputTo(target)
                            )
                        )
                    ).value();
                    Logger.debug(
                        HmBase.class, "File %s saved (%d bytes)",
                        target, bytes
                    );
                } catch (final IOException ex) {
                    throw new IOException(
                        String.format(
                            "Failed while trying to save to %s",
                            target
                        ),
                        ex
                    );
                }
            }
        );
    }

    @Override
    public void save(final String str, final Path path) throws IOException {
        this.origin.save(str, path);
    }

    @Override
    public void save(final Text txt, final Path path) throws IOException {
        this.origin.save(txt, path);
    }

    @Override
    public void save(final InputStream stream, final Path path) throws IOException  {
        this.origin.save(stream, path);
    }

    @Override
    public void save(final byte[] bytes, final Path path) throws IOException  {
        this.origin.save(bytes, path);
    }

    @Override
    public void save(final Input input, final Path path) throws IOException {
        this.origin.save(input, path);
    }

    @Override
    public boolean exists(final Path path) {
        return Files.exists(this.absolute(this.onlyRelative(path)));
    }

    @Override
    public Bytes load(final Path path) throws IOException {
        return new BytesOf(Files.readAllBytes(this.absolute(this.onlyRelative(path))));
    }

    @Override
    public Path absolute(final Path path) {
        return this.cwd.resolve(path);
    }

    @Override
    public Path onlyRelative(final Path path) {
        if (path.isAbsolute()) {
            throw new IllegalArgumentException(
                String.format(
                    "Path must be relative to base %s but absolute given: %s",
                    this.cwd,
                    path
                )
            );
        }
        return path;
    }
}
