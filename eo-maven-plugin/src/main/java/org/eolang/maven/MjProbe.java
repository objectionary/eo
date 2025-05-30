/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.iterable.IterableOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;

/**
 * Go through all `probe` and `also` metas in XMIR files, try to locate the
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
public final class MjProbe extends MjSafe {
    /**
     * Shift that adds probes.
     */
    private static final Shift ADD_PROBES = new StClasspath(
        "/org/eolang/maven/probe/add-probes.xsl"
    );

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
    private void probe() {
        final Collection<TjForeign> tojos = this.scopedTojos().unprobed();
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
        } else {
            final long start = System.currentTimeMillis();
            final Map<String, Boolean> probed = new ConcurrentHashMap<>(0);
            if (this.probed(tojos, probed) == 0) {
                Logger.info(this, "No probes found in %d programs", tojos.size());
            } else {
                Logger.info(
                    this, "Found %d probe(s) in %d program(s) in %[ms]s: %s",
                    probed.size(), tojos.size(),
                    System.currentTimeMillis() - start,
                    probed.keySet()
                );
            }
        }
    }

    /**
     * Probe given tojos and return amount of probed objects.
     * @param tojos Tojos
     * @param probed Probed objects
     * @return Amount of probed objects
     */
    private int probed(final Collection<TjForeign> tojos, final Map<String, Boolean> probed) {
        return new Threaded<>(
            tojos,
            tojo -> {
                final Path src = tojo.xmir();
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
                    probed.put(object, true);
                }
                tojo.withProbed(count);
                return count;
            }
        ).total();
    }

    /**
     * Find all probes found in the provided XML file.
     *
     * @param file The .xmir file
     * @return List of foreign objects found
     */
    private Collection<String> probes(final Path file) throws FileNotFoundException {
        final long start = System.currentTimeMillis();
        final Collection<String> objects = new ListOf<>(
            new Mapped<>(
                MjProbe::noPrefix,
                MjProbe.metas(file)
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
     * "/object/metas/meta[head/text()='probe' or head/text()='also']/tail[not(text()='')]/text()"
     * @param file XML file
     * @return Metas to probe
     */
    private static Iterable<String> metas(final Path file) throws FileNotFoundException {
        return new IterableOf<>(
            new Xnav(MjProbe.ADD_PROBES.apply(0, new XMLDocument(file)).inner())
                .element("object")
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
                    .map(meta -> meta.element("tail").text().orElse(""))
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
