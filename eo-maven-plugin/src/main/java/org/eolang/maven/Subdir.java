/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import io.github.sekator778.seqdir.Seqdir;
import io.github.sekator778.seqdir.Sequence;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.util.Optional;

/**
 * A numbered subdirectory of {@code target/eo}.
 *
 * <p>No stage picks its own number any more. A name that already owns a
 * {@code NN-name} directory under {@code target} keeps that number, found
 * by reading the directory itself rather than by replaying how this build
 * reached it; a name with none yet is given the number past the highest one
 * already taken, and that empty directory is created on the spot so the
 * reservation is visible to whoever asks next, in this build or a later
 * one. A stage that this build never reaches because an earlier one was
 * cached therefore does not shift the numbers a later build gives to the
 * stages that do run, and the same {@code target} never grows two
 * directories for the same name.</p>
 *
 * <p>The look-up and the reservation are done by
 * <a href="https://github.com/Sekator778/seqdir">seqdir</a>, which keeps two
 * threads of one build and two Maven processes apart, so that two names never
 * claim the same number and one name never gets two (see #9013).</p>
 *
 * @since 0.72.0
 */
final class Subdir {

    /**
     * The {@code target/eo} directory this subdirectory lives under.
     */
    private final Path target;

    /**
     * The name of this subdirectory, without its numeric prefix.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param tgt The {@code target/eo} directory this subdirectory lives under
     * @param nme The name of this subdirectory, without its numeric prefix
     */
    Subdir(final File tgt, final String nme) {
        this(tgt.toPath(), nme);
    }

    /**
     * Ctor.
     *
     * @param tgt The {@code target/eo} directory this subdirectory lives under
     * @param nme The name of this subdirectory, without its numeric prefix
     */
    Subdir(final Path tgt, final String nme) {
        this.target = tgt;
        this.name = nme;
    }

    /**
     * The path of this subdirectory, unless a mojo parameter already
     * names one to use instead.
     *
     * @param configured The value of the parameter, or null when unset
     * @return The path to use
     */
    Path orConfigured(final File configured) {
        final Path path;
        if (configured == null) {
            path = this.path();
        } else {
            path = configured.toPath();
        }
        return path;
    }

    /**
     * The path of this subdirectory as the disk already has it, unless a
     * mojo parameter already names one to use instead.
     *
     * @param configured The value of the parameter, absent when unset
     * @return The path to read
     */
    Path foundOrConfigured(final File configured) {
        return Optional.ofNullable(configured).map(File::toPath).orElseGet(this::found);
    }

    /**
     * The path of this subdirectory as the disk already has it.
     *
     * <p>Nothing is created and no number is reserved, unlike
     * {@link #path()}, so a goal that only reads a stage leaves no empty
     * directory behind. A stage with no directory yet gets its unnumbered
     * path under the target, which no stage ever occupies, so that the
     * caller finds it absent and says so.</p>
     *
     * @return The path, which is no directory when the stage never ran
     */
    Path found() {
        try {
            return this.dirs().find(this.name)
                .orElseGet(() -> this.target.resolve(this.name));
        } catch (final IOException ex) {
            throw new UncheckedIOException(
                String.format(
                    "Failed to look for '%s' under %s", this.name, this.target
                ),
                ex
            );
        }
    }

    /**
     * The path of this subdirectory.
     *
     * @return The path
     */
    Path path() {
        try {
            return this.dirs().once(this.name);
        } catch (final IOException ex) {
            throw new UncheckedIOException(
                String.format(
                    "Failed to number '%s' under %s", this.name, this.target
                ),
                ex
            );
        }
    }

    private Sequence dirs() {
        return new Seqdir(this.target, 2).dirs();
    }
}
