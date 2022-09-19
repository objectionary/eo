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

/**
 * Class for saving and loading files.
 *
 * @since 0.27
 * @todo #1105:30min create load function (it has to be able read by path)
 *  and add new unit test for added load function
 */
@SuppressWarnings("PMD.TooManyMethods")
public class Home {
    /**
     * Current working directory.
     */
    private final Path cwd;

    /**
     * Ctor.
     */
    Home() {
        this(Paths.get(""));
    }

    /**
     * Ctor.
     * @param path Path
     * @todo #1105:30min we need to modify function. It has to change
     *  path by replacing bad characters (not utf-8) or maybe
     *  we can invent any other solution
     */
    Home(final Path path) {
        this.cwd = path;
    }

    /**
     * Saving input.
     * @param input Input
     * @param path Path
     * @return Saved bytes
     * @throws IOException If fails
     */
    public long save(final Input input, final Path path) throws IOException {
        final File dir = path.toFile().getParentFile();
        if (dir.mkdirs()) {
            Logger.debug(
                this, "Directory created: %s",
                this.rel(path.getParent())
            );
        }
        try {
            final long bytes = new IoChecked<>(
                new LengthOf(
                    new TeeInput(
                        input,
                        new OutputTo(path)
                    )
                )
            ).value();
            Logger.debug(
                Home.class, "File %s saved (%.0f bytes)",
                this.rel(path), bytes
            );
            return bytes;
        } catch (final IOException ex) {
            throw new IOException(
                String.format(
                    "Failed while trying to save to %s",
                    this.rel(path)
                ),
                ex
            );
        }
    }

    /**
     * Saving string.
     * @param str String
     * @param path Path
     * @return Saved bytes
     * @throws IOException If fails
     */
    public long save(final String str, final Path path) throws IOException {
        return this.save(new InputOf(str), path);
    }

    /**
     * Saving text.
     * @param txt Text
     * @param path Path
     * @return Saved bytes
     * @throws IOException If fails
     */
    public long save(final Text txt, final Path path) throws IOException {
        return this.save(new InputOf(txt), path);
    }

    /**
     * Saving stream.
     * @param stream Input stream
     * @param path Path
     * @return Saved bytes
     * @throws IOException If fails
     */
    public long save(final InputStream stream, final Path path) throws IOException {
        return this.save(new InputOf(stream), path);
    }

    /**
     * Saving bytes.
     * @param bytes Byte array
     * @param path Path
     * @return Saved bytes
     * @throws IOException If fails
     */
    public long save(final byte[] bytes, final Path path) throws IOException {
        return this.save(new InputOf(bytes), path);
    }

    /**
     * Saving string.
     * @param str String
     * @param path Path
     * @return Saved bytes
     * @todo We should remove this function and get rid of using it
     *  because it is not really better than simple save
     */
    public long saveQuietly(final String str, final Path path) {
        try {
            return this.save(new InputOf(str), path);
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Make relative name from path.
     * @param file The path of the file or dir
     * @return Relative name to CWD
     */
    public String rel(final Path file) {
        String path = file.toAbsolutePath().toString();
        if (path.equals(this.cwd.toString())) {
            path = "./";
        } else if (path.startsWith(this.cwd.toString())) {
            path = String.format(
                "./%s",
                path.substring(this.cwd.toString().length() + 1)
            );
        }
        return path;
    }
}
