/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
final class HmSave implements Home {
    /**
     * BiProc with two arguments for saving {@link Input} from first argument to file from second.
     */
    private final IoCheckedBiProc<Input, Path> sve;

    /**
     * Ctor.
     *
     * @param save BiProc for saving {@link Input} to file.
     */
    HmSave(final BiProc<Input, Path> save) {
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
