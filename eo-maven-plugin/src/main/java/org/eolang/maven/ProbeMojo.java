/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;
import java.util.stream.Stream;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.iterable.IterableOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;

/**
 * Go through all `probe` metas in XMIR files, try to locate the
 * objects pointed by `probe` in Objectionary, and if found, register them in
 * the catalog.
 * More about the purpose of this Mojo is in
 * <a href="https://github.com/objectionary/eo/issues/1323">this issue</a>.
 *
 * @since 0.28.11
 */
@Mojo(
    name = "probe",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class ProbeMojo extends SafeMojo {
    /**
     * Objectionary.
     * @since 0.50
     * @checkstyle MemberNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Objectionary objectionary = new OyIndexed(
        new OyRemote(this.hash)
    );

    @Override
    public void exec() throws IOException {
        if (this.offline) {
            Logger.info(
                this,
                "No programs were probed because eo.offline flag is TRUE"
            );
        } else {
            this.probe();
        }
    }

    /**
     * Probe objects.
     */
    private void probe() throws IOException {
        final long start = System.currentTimeMillis();
        final Collection<String> probed = new HashSet<>(0);
        final Collection<TjForeign> tojos = this.scopedTojos().unprobed();
        for (final TjForeign tojo : tojos) {
            final Path src = tojo.shaken();
            final Collection<String> objects = this.probes(src);
            if (!objects.isEmpty()) {
                Logger.debug(this, "Probing object(s): %s", objects);
            }
            int count = 0;
            for (final String object : objects) {
                if (!this.objectionary.contains(object)) {
                    continue;
                }
                ++count;
                this.scopedTojos().add(object).withDiscoveredAt(src);
                probed.add(object);
            }
            tojo.withProbed(count);
        }
        if (tojos.isEmpty()) {
            if (this.scopedTojos().size() == 0) {
                Logger.warn(this, "Nothing to probe, since there are no programs");
            } else {
                Logger.info(
                    this,
                    "Nothing to probe, all %d programs checked already",
                    this.scopedTojos().size()
                );
            }
        } else if (probed.isEmpty()) {
            Logger.info(this, "No probes found in %d programs", tojos.size());
        } else {
            Logger.info(
                this, "Found %d probe(s) in %d program(s) in %[ms]s: %s",
                probed.size(), tojos.size(),
                System.currentTimeMillis() - start,
                probed
            );
        }
    }

    /**
     * Find all probes found in the provided XML file.
     *
     * @param file The .xmir file
     * @return List of foreign objects found
     */
    private Collection<String> probes(final Path file) {
        final long start = System.currentTimeMillis();
        final Collection<String> objects = new ListOf<>(
            new Mapped<>(
                ProbeMojo::noPrefix,
                ProbeMojo.metas(file)
            ).iterator()
        );
        if (objects.isEmpty()) {
            Logger.debug(this, "Didn't find any probed objects in %[file]s", file);
        } else {
            Logger.debug(
                this,
                "Found %d probed objects in %[file]s in %[ms]s: %s",
                objects.size(), file,
                System.currentTimeMillis() - start,
                objects
            );
        }
        return objects;
    }

    /**
     * Return metas for probing.
     * The equivalent xpath is:
     * "/program/metas/meta[head/text()='probe' or head/text()='also']/tail[not(text()='')]/text()"
     * @param file XML file
     * @return Metas to probe
     */
    private static Iterable<String> metas(final Path file) {
        return new IterableOf<>(
            new Xnav(file)
                .element("program")
                .elements(Filter.withName("metas"))
                .findFirst()
                .map(
                    metas -> metas.elements(
                        Filter.all(
                            Filter.withName("meta"),
                            meta -> {
                                final Optional<String> head = new Xnav(meta)
                                    .element("head")
                                    .text();
                                return head.map(
                                    text -> "probe".equals(text) || "also".equals(text)
                                ).orElse(false);
                            }
                        )
                    )
                    .map(meta -> meta.element("tail").text().get())
                    .filter(meta -> !meta.isEmpty())
                )
                .orElse(Stream.of())
                .iterator()
        );
    }

    /**
     * Trim Q prefix.
     * Q.a.b.c -> a.b
     * a.b.c -> a.b.c
     * @param obj Full object name
     * @return Trimmed object name
     */
    private static String noPrefix(final String obj) {
        final String result;
        if (obj.length() > 1 && "Q.".equals(obj.substring(0, 2))) {
            result = obj.substring(2);
        } else {
            result = obj;
        }
        return result;
    }
}
