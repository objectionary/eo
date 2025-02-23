/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import org.cactoos.Bytes;
import org.cactoos.Input;
import org.cactoos.Text;

/**
 * Location for files that saves optionally.
 * @since 0.32.0
 */
@SuppressWarnings("PMD.TooManyMethods")
final class HmOptional implements Home {
    /**
     * Original home.
     */
    private final Home origin;

    /**
     * Home with "save" functionality.
     */
    private final Home sve;

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
    HmOptional(final Home home, final boolean rwte) {
        this.origin = home;
        this.rewrite = rwte;
        this.sve = new HmSave(
            (input, path) -> {
                final Path target = this.absolute(this.onlyRelative(path));
                if (!target.toFile().exists() || this.rewrite) {
                    this.origin.save(input, path);
                } else {
                    Logger.info(this, "Rewriting of the %s file was skipped", target);
                }
            }
        );
    }

    @Override
    public void save(final String str, final Path path) throws IOException {
        this.sve.save(str, path);
    }

    @Override
    public void save(final Text txt, final Path path) throws IOException {
        this.sve.save(txt, path);
    }

    @Override
    public void save(final InputStream stream, final Path path) throws IOException  {
        this.sve.save(stream, path);
    }

    @Override
    public void save(final byte[] bytes, final Path path) throws IOException  {
        this.sve.save(bytes, path);
    }

    @Override
    public void save(final Input input, final Path path) throws IOException {
        this.sve.save(input, path);
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
