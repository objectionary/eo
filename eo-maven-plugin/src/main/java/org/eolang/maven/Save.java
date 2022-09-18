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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;
import org.cactoos.text.TextOf;

/**
 * Save a file operation.
 *
 * @since 0.1
 */
public final class Save {

    /**
     * Content.
     */
    private final Input content;

    /**
     * Path.
     */
    private final Path path;

    /**
     * Ctor.
     * @param input The input
     * @param file File to save to
     */
    public Save(final InputStream input, final Path file) {
        this(new InputOf(input), file);
    }

    /**
     * Ctor.
     *
     * @param txt The content
     * @param file The path
     */
    public Save(final String txt, final Path file) {
        this(new TextOf(txt), file);
    }

    /**
     * Ctor.
     *
     * @param txt The content
     * @param file The path
     */
    public Save(final Text txt, final Path file) {
        this(new InputOf(txt), file);
    }

    /**
     * Ctor.
     *
     * @param bytes The content
     * @param file The path
     */
    public Save(final byte[] bytes, final Path file) {
        this(new InputOf(bytes), file);
    }

    /**
     * Ctor.
     * @param input The input
     * @param file File to save to
     */
    public Save(final Input input, final Path file) {
        this.content = input;
        this.path = file;
    }

    /**
     * Save the file to the path, without any checked exceptions.
     */
    public void saveQuietly() {
        try {
            this.save();
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Save the file to the path.
     *
     * @throws IOException If fails
     */
    public void save() throws IOException {
        final File dir = this.path.toFile().getParentFile();
        if (dir.mkdirs()) {
            Logger.debug(
                Save.class, "Directory created: %s",
                Save.rel(this.path.getParent())
            );
        }
        try {
            final double bytes = new IoChecked<>(
                new LengthOf(
                    new TeeInput(
                        this.content,
                        new OutputTo(this.path)
                    )
                )
            ).value();
            Logger.debug(
                this, "File %s saved (%.0f bytes)",
                Save.rel(this.path), bytes
            );
        } catch (final IOException ex) {
            throw new IOException(
                String.format(
                    "Failed while trying to save to %s",
                    Save.rel(this.path)
                ),
                ex
            );
        }
    }

    /**
     * Make relative name from path.
     * @param file The path of the file or dir
     * @return Relative name to CWD
     */
    @SuppressWarnings("PMD.ProhibitPublicStaticMethods")
    public static String rel(final Path file) {
        final String cwd = Paths.get("").toAbsolutePath().toString();
        String path = file.toAbsolutePath().toString();
        if (path.equals(cwd)) {
            path = "./";
        } else if (path.startsWith(cwd)) {
            path = String.format(
                "./%s",
                path.substring(cwd.length() + 1)
            );
        }
        return path;
    }

}
