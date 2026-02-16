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
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Pull EO files from Objectionary.
 * <p>
 *     This goal goes through all objects from "foreign" catalog and looks for those without
 *     sources and pulls them from Objectionary remote repository.
 *     The pulled sources are stored in the {@link #DIR} directory.
 * </p>
 * @since 0.1
 */
@Mojo(
    name = "pull",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjPull extends MjSafe {
    /**
     * The directory where to process to.
     */
    static final String DIR = "2-pull";

    /**
     * Cache directory.
     */
    static final String CACHE = "pulled";

    @Override
    public void exec() throws IOException {
        if (this.offline) {
            Logger.info(
                this,
                "No programs were pulled because eo.offline flag is TRUE"
            );
        } else {
            this.pull(
                new OyIndexed(
                    new OyCached(new OyRemote(this.hash, this.proxies()))
                )
            );
        }
    }

    /**
     * Pull them all.
     * @param objectionary Objectionary to pull from
     * @throws IOException If fails
     */
    private void pull(final OyIndexed objectionary) throws IOException {
        final long start = System.currentTimeMillis();
        final Collection<TjForeign> tojos = this.scopedTojos().withoutSources();
        final Collection<String> names = new ArrayList<>(0);
        for (final TjForeign tojo : tojos) {
            final String object = tojo.identifier();
            if (objectionary.isDirectory(object)) {
                continue;
            }
            try {
                tojo.withSource(
                    this.pulled(
                        object,
                        this.targetDir.toPath().resolve(MjPull.DIR),
                        this.hash.value()
                    )
                ).withHash(new ChNarrow(this.hash));
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
        if (tojos.isEmpty()) {
            Logger.info(
                this,
                "No programs were pulled in %[ms]s",
                System.currentTimeMillis() - start
            );
        } else {
            Logger.info(
                this,
                "%d program(s) were pulled in %[ms]s: %s",
                tojos.size(),
                System.currentTimeMillis() - start,
                names
            );
        }
    }

    /**
     * Pull one object.
     * @param object Name of the object
     * @param base Base cache path
     * @param hsh Git hash
     * @return The path of .eo file
     * @throws IOException If fails
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private Path pulled(
        final String object,
        final Path base,
        final String hsh) throws IOException {
        final Path target = new Place(object).make(base, MjAssemble.EO);
        final Supplier<Path> che = new CachePath(
            this.cache.toPath().resolve(MjPull.CACHE),
            this.plugin.getVersion(),
            hsh,
            base.relativize(target)
        );
        final Footprint generated = new FpGenerated(
            src -> {
                Logger.debug(
                    this,
                    "Pulling %s object from remote objectionary with hash %s",
                    object, hsh
                );
                return this.objectionary().get(object);
            }
        );
        final Footprint both = new FpUpdateBoth(generated, che);
        return new FpIfReleased(
            hsh,
            new FpFork(
                (src, tgt) -> {
                    if (this.overWrite) {
                        Logger.debug(
                            this,
                            "Pulling sources again because \"eo.overWrite=TRUE\""
                        );
                    }
                    return this.overWrite;
                },
                new FpFork(this.cacheEnabled, both, generated),
                new FpIfTargetExists(
                    new FpIgnore(),
                    new FpFork(
                        this.cacheEnabled,
                        new FpIfTargetExists(tgt -> che.get(), new FpUpdateFromCache(che), both),
                        generated
                    )
                )
            ),
            generated
        ).apply(Paths.get(""), target);
    }
}
