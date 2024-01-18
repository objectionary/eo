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
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import org.cactoos.Bytes;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.io.InputOf;

/**
 * Location for files that saves optionally.
 * @since 0.32.0
 */
@SuppressWarnings("PMD.TooManyMethods")
public final class HmOptional implements Home {
    /**
     * Original home.
     */
    private final Home origin;

    /**
     * Rewrite files or not.
     */
    private final boolean rewrite;

    /**
     * Ctor.
     *
     * @param home Origin home
     * @param rwte Rewrite or not
     */
    public HmOptional(final Home home, final boolean rwte) {
        this.origin = home;
        this.rewrite = rwte;
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
        if (!target.toFile().exists() || this.rewrite) {
            this.origin.save(input, path);
        } else {
            Logger.info(this, "Rewriting of the %s file was skipped", target);
        }
    }

    @Override
    public boolean exists(final Path path) {
        return this.origin.exists(path);
    }

    @Override
    public Bytes load(final Path path) throws IOException {
        return this.origin.load(path);
    }

    @Override
    public Path absolute(final Path path) {
        return this.origin.absolute(path);
    }

    @Override
    public Path onlyRelative(final Path path) {
        return this.origin.onlyRelative(path);
    }
}
