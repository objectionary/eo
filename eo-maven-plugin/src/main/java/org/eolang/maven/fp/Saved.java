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
package org.eolang.maven.fp;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.Input;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;
import org.cactoos.text.TextOf;

/**
 * Content saved to the file.
 * Returns path to the file
 * @since 0.41.0
 */
public final class Saved implements Scalar<Path> {
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
    public Saved(final String content, final Path target) {
        this(new InputOf(content), target);
    }

    /**
     * Ctor.
     * @param content Content as scalar
     * @param target Path to save content to
     */
    public Saved(final Scalar<String> content, final Path target) {
        this(new TextOf(content), target);
    }

    /**
     * Ctor.
     * @param content Content as text
     * @param target Path to save content to
     */
    public Saved(final Text content, final Path target) {
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
