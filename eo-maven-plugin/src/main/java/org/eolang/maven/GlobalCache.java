/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.BiFunc;
import org.cactoos.Func;
import org.cactoos.io.InputOf;

/**
 * The cache shared by every build on this machine.
 *
 * <p>A step asks it how one file is to be written and gets a {@link Footprint}
 * back. Whether that footprint goes through the cache or straight past it is
 * decided once, where the {@code eo.cacheEnabled} option is read, so no step
 * has to know that the option exists.</p>
 *
 * @since 0.74
 */
interface GlobalCache {

    /**
     * How one file is to be written.
     * @param tail Path of that file inside the cache
     * @param hash Hash segment of the cache path
     * @param compile How the content of the file is produced
     * @return The footprint that writes it
     */
    Footprint footprint(Path tail, Supplier<String> hash, Func<Path, String> compile);

    /**
     * How one file, already made by a footprint of its own, is to be kept.
     *
     * <p>Unlike {@link #footprint(Path, Supplier, Func)}, the content does not
     * come from a source file this cache can fingerprint: the caller knows on
     * its own whether the target is out of date and says so through
     * {@code rewrite}.</p>
     *
     * @param tail Path of that file inside the cache
     * @param hash Hash segment of the cache path
     * @param rewrite Whether the target has to be written again
     * @param made How the file is produced when the cache has nothing
     * @return The footprint that writes it
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    Footprint kept(
        Path tail, Supplier<String> hash, BiFunc<Path, Path, Boolean> rewrite, Footprint made
    );

    /**
     * The same cache, with one more segment folded into its key.
     * @param segment The segment to fold in
     * @return The derived cache
     */
    GlobalCache with(String segment);

    /**
     * The cache that remembers nothing.
     *
     * <p>Every file is produced and written to its target, and the cache
     * directory is neither read nor written. This is what the build gets when
     * {@code eo.cacheEnabled} is false.</p>
     *
     * @since 0.74
     */
    final class GcFresh implements GlobalCache {

        @Override
        public Footprint footprint(
            final Path tail, final Supplier<String> hash, final Func<Path, String> compile
        ) {
            return new FpGenerated(src -> new InputOf(compile.apply(src)));
        }

        @Override
        public Footprint kept(
            final Path tail,
            final Supplier<String> hash,
            final BiFunc<Path, Path, Boolean> rewrite,
            final Footprint made
        ) {
            return new FpFork(
                rewrite, made, new FpIfTargetExists(new FpIgnore(), made)
            );
        }

        @Override
        public GlobalCache with(final String segment) {
            return this;
        }
    }
}
