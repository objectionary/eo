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

import com.jcabi.log.Logger;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
 * @todo #1352:30min Prohibit absolute paths in methods `save`, `load`, `exists`.
 *  Throw an exception in case absolut path is given for these methods.
 */
@SuppressWarnings("PMD.TooManyMethods")
public final class Home {
    /**
     * Current working directory.
     */
    private final Path cwd;

    /**
     * Ctor.
     */
    public Home() {
        this(Paths.get(""));
    }

    /**
     * Ctor.
     *
     * @param path Path
     */
    public Home(final Path path) {
        this.cwd = path;
    }

    /**
     * Saving input.
     *
     * @param input Input
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    public void save(final Input input, final Path path) throws IOException {
        final Path target = this.absolute(path);
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
                Home.class, "File %s saved (%d bytes)",
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

    /**
     * Saving string.
     *
     * @param str String
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    public void save(final String str, final Path path) throws IOException {
        this.save(new InputOf(str), path);
    }

    /**
     * Saving text.
     *
     * @param txt Text
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    public void save(final Text txt, final Path path) throws IOException {
        this.save(new InputOf(txt), path);
    }

    /**
     * Saving stream.
     *
     * @param stream Input stream
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    public void save(final InputStream stream, final Path path) throws IOException {
        this.save(new InputOf(stream), path);
    }

    /**
     * Saving bytes.
     *
     * @param bytes Byte array
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    public void save(final byte[] bytes, final Path path) throws IOException {
        this.save(new InputOf(bytes), path);
    }

    /**
     * Check if exists.
     *
     * @param path Cwd-relative path to file
     * @return True if exists
     */
    public boolean exists(final Path path) {
        return Files.exists(this.absolute(path));
    }

    /**
     * Load bytes from file by path.
     *
     * @param path Cwd-relative path to file
     * @return Bytes of file
     * @throws IOException if method can't find the file by path or
     *  if some exception happens during reading the file
     */
    public Bytes load(final Path path) throws IOException {
        return new BytesOf(Files.readAllBytes(this.absolute(path)));
    }

    /**
     * Absolute path to a file.
     *
     * @param path Cwd-relative path to file
     * @return Absolute path
     */
    public Path absolute(final Path path) {
        return this.cwd.resolve(path);
    }
}
