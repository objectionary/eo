/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Supplier;

/**
 * Pulls EO sources from Objectionary.
 *
 * <p>
 *     Goes through all objects from "foreign" catalog that lack sources and
 *     pulls them from the Objectionary remote repository. The pulled sources
 *     are stored in the {@link #DIR} directory.
 * </p>
 *
 * @since 0.61.0
 */
final class Pulling implements Step {

    /**
     * The directory where to store pulled sources.
     */
    static final String DIR = "2-pull";

    /**
     * Cache subdirectory name.
     */
    static final String CACHE = "pulled";

    /**
     * Foreign tojos catalog.
     */
    private final TjsForeign tojos;

    /**
     * Base target directory (targetDir + DIR).
     */
    private final Path base;

    /**
     * Commit hash to pull from.
     */
    private final CommitHash hash;

    /**
     * Objectionary to pull from.
     */
    private final Objectionary objectionary;

    /**
     * Cache directory (cache + CACHE).
     */
    private final Path cdir;

    /**
     * Plugin version string.
     */
    private final String version;

    /**
     * Whether to overwrite already pulled sources.
     */
    private final boolean overwrite;

    /**
     * Whether caching is enabled.
     */
    private final boolean cenabled;

    /**
     * Whether we are in offline mode.
     */
    private final boolean offline;

    /**
     * Constructor.
     * @param tjs Foreign tojos catalog
     * @param dir Base target directory
     * @param hsh Commit hash
     * @param obj Objectionary
     * @param cache Cache directory
     * @param ver Plugin version
     * @param rewrite Whether to overwrite existing sources
     * @param enabled Whether caching is enabled
     * @param off Whether offline mode is active
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Pulling(
        final TjsForeign tjs,
        final Path dir,
        final CommitHash hsh,
        final Objectionary obj,
        final Path cache,
        final String ver,
        final boolean rewrite,
        final boolean enabled,
        final boolean off
    ) {
        this.tojos = tjs;
        this.base = dir;
        this.hash = hsh;
        this.objectionary = obj;
        this.cdir = cache;
        this.version = ver;
        this.overwrite = rewrite;
        this.cenabled = enabled;
        this.offline = off;
    }

    /**
     * Pull all objects.
     * @throws IOException If fails
     */
    @Override
    public void exec() throws IOException {
        if (this.offline) {
            Logger.info(
                this,
                "No programs were pulled because eo.offline flag is TRUE"
            );
            return;
        }
        final long start = System.currentTimeMillis();
        final Collection<TjForeign> sources = this.tojos.withoutSources();
        final Collection<String> names = new ArrayList<>(0);
        final String hsh = this.hash.value();
        for (final TjForeign tojo : sources) {
            final String object = tojo.identifier();
            if (this.objectionary.isDirectory(object)) {
                continue;
            }
            try {
                tojo.withSource(this.pulled(object, hsh))
                    .withHash(new ChNarrow(this.hash));
            } catch (final IOException exception) {
                throw new IOException(
                    String.format(
                        "Failed to pull '%s' earlier discovered at %s",
                        tojo.identifier(),
                        tojo.discoveredAt()
                    ),
                    exception
                );
            }
            names.add(object);
        }
        if (sources.isEmpty()) {
            Logger.info(
                this,
                "No programs were pulled in %[ms]s",
                System.currentTimeMillis() - start
            );
        } else {
            Logger.info(
                this,
                "%d program(s) were pulled in %[ms]s: %s",
                sources.size(),
                System.currentTimeMillis() - start,
                names
            );
        }
    }

    /**
     * Pull one object.
     * @param object Name of the object
     * @param hsh Git hash
     * @return The path of .eo file
     * @throws IOException If fails
     */
    private Path pulled(final String object, final String hsh) throws IOException {
        final Path target = new Place(object).make(this.base, MjAssemble.EO);
        final Supplier<Path> che = new CachePath(
            this.cdir,
            this.version,
            hsh,
            this.base.relativize(target)
        );
        final Footprint generated = new FpGenerated(
            src -> {
                Logger.debug(
                    this,
                    "Pulling %s object from remote objectionary with hash %s",
                    object, hsh
                );
                return this.objectionary.get(object);
            }
        );
        final Footprint both = new FpUpdateBoth(generated, che);
        return new FpIfReleased(
            hsh,
            new FpFork(
                (src, tgt) -> {
                    if (this.overwrite) {
                        Logger.debug(
                            this,
                            "Pulling sources again because \"eo.overWrite=TRUE\""
                        );
                    }
                    return this.overwrite;
                },
                new FpFork(this.cenabled, both, generated),
                new FpIfTargetExists(
                    new FpIgnore(),
                    new FpFork(
                        this.cenabled,
                        new FpIfTargetExists(tgt -> che.get(), new FpUpdateFromCache(che), both),
                        generated
                    )
                )
            ),
            generated
        ).apply(Paths.get(""), target);
    }
}
