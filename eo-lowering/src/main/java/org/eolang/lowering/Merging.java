/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.nio.file.Path;

/**
 * The merging of every object of the build into one phi-expression.
 *
 * <p>The calculus knows nothing of files. A formation that copies an
 * object of another file has to find that object where it stands, so the
 * boxed files and the entries are joined into a single document, and it
 * is that document, and never a file of it, that the evaluation is asked
 * about. The tests of an object stay in it, since an object and what is
 * said about it are one document in this compiler.</p>
 *
 * @since 0.74.0
 * @todo #8548:45min Join the boxed files and {@code entries.xmir} into
 *  {@code world.phi} in the lowering directory, with one call of
 *  {@code phino merge --input=xmir}, and fail the build when that call
 *  comes back with an exit code other than zero, quoting what the binary
 *  printed, since a world that was not merged cannot be evaluated and
 *  there is nothing sensible for a later stage to do about it.
 */
final class Merging implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     */
    Merging(final Path dir) {
        this.home = dir;
    }

    @Override
    public void exec() {
        Logger.debug(this, "No world is merged yet in %s", this.home);
    }
}
