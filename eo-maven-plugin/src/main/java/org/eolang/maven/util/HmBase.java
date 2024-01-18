/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.util;

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
import org.cactoos.io.InputOf;
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
public final class HmBase implements Home {
    /**
     * Current working directory.
     */
    private final Path cwd;

    /**
     * Ctor.
     *
     * @param file File
     */
    public HmBase(final File file) {
        this(file.toPath());
    }

    /**
     * Ctor.
     *
     * @param path Path
     */
    public HmBase(final Path path) {
        this.cwd = path;
    }

    @Override
    public void save(final String str, final Path path) throws IOException {
        this.save(new InputOf(str), path);
    }

    @Override
    public void save(final Text txt, final Path path) throws IOException {
        this.save(new InputOf(txt), path);
    }

    @Override
    public void save(final InputStream stream, final Path path) throws IOException {
        this.save(new InputOf(stream), path);
    }

    @Override
    public void save(final byte[] bytes, final Path path) throws IOException {
        this.save(new InputOf(bytes), path);
    }

    @Override
    public void save(final Input input, final Path path) throws IOException {
        final Path target = this.absolute(this.onlyRelative(path));
        if (target.toFile().getParentFile().mkdirs()) {
            Logger.debug(
                this, "Directory created: %s",
                new Rel(target.getParent())
            );
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
