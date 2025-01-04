/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import org.cactoos.BiProc;
import org.cactoos.Bytes;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.proc.IoCheckedBiProc;

/**
 * Home that defines the logic of saving different types of data to files.
 *
 * @since 0.37.0
 */
public final class HmSave implements Home {
    /**
     * BiProc with two arguments for saving {@link Input} from first argument to file from second.
     */
    private final IoCheckedBiProc<Input, Path> sve;

    /**
     * Ctor.
     *
     * @param save BiProc for saving {@link Input} to file.
     */
    public HmSave(final BiProc<Input, Path> save) {
        this.sve = new IoCheckedBiProc<>(save);
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
        this.sve.exec(input, path);
    }

    @Override
    public boolean exists(final Path path) {
        throw new IllegalStateException("Should never happen");
    }

    @Override
    public Bytes load(final Path path) throws IOException {
        throw new IllegalStateException("Should never happen");
    }

    @Override
    public Path absolute(final Path path) {
        throw new IllegalStateException("Should never happen");
    }

    @Override
    public Path onlyRelative(final Path path) {
        throw new IllegalStateException("Should never happen");
    }
}
