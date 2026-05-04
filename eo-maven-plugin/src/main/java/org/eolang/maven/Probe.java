/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.list.ListOf;

/**
 * Goes through all {@code probe} and {@code also} metas in XMIR files,
 * tries to locate the objects pointed by {@code probe} in Objectionary,
 * and if found, registers them in the catalog.
 *
 * @since 0.67.0
 */
final class Probe {

    /**
     * Tojos to probe.
     */
    private final TjsForeign tojos;

    /**
     * Objectionary to check for objects.
     */
    private final Objectionary objectionary;

    /**
     * Whether we are online.
     */
    private final boolean online;

    /**
     * Constructor.
     * @param tjs Tojos
     * @param obj Objectionary
     * @param net Whether we are online
     */
    Probe(final TjsForeign tjs, final Objectionary obj, final boolean net) {
        this.tojos = tjs;
        this.objectionary = obj;
        this.online = net;
    }

    /**
     * Run probing.
     * @throws IOException If fails
     */
    void exec() throws IOException {
        if (this.online) {
            final Collection<TjForeign> unprobed = this.tojos.unprobed();
            if (unprobed.isEmpty()) {
                if (this.tojos.size() == 0) {
                    Logger.warn(this, "Nothing to probe, since there are no programs");
                } else {
                    Logger.info(
                        this,
                        "Nothing to probe, all %d programs checked already",
                        this.tojos.size()
                    );
                }
            } else {
                final long start = System.currentTimeMillis();
                final Map<String, Boolean> probed = new ConcurrentHashMap<>(0);
                if (this.probed(unprobed, probed) == 0) {
                    Logger.info(this, "No probes found in %d programs", unprobed.size());
                } else {
                    Logger.info(
                        this, "Found %d probe(s) in %d program(s) in %[ms]s: %s",
                        probed.size(), unprobed.size(),
                        System.currentTimeMillis() - start,
                        probed.keySet()
                    );
                }
            }
        } else {
            Logger.info(
                this,
                "No programs were probed because eo.offline flag is TRUE"
            );
        }
    }

    /**
     * Probe given tojos and return amount of probed objects.
     * @param unprobed Tojos to probe
     * @param probed Map accumulating discovered probes
     * @return Amount of probed objects
     */
    private int probed(
        final Collection<TjForeign> unprobed,
        final Map<String, Boolean> probed) {
        return new Threaded<>(
            unprobed,
            tojo -> {
                final Path src = tojo.xmir();
                final Collection<String> objects = new ListOf<>(new Probes(src));
                if (!objects.isEmpty()) {
                    Logger.debug(this, "Probing object(s): %s", objects);
                }
                int count = 0;
                for (final String object : objects) {
                    if (!this.objectionary.contains(object)) {
                        continue;
                    }
                    ++count;
                    this.tojos.add(object).withDiscoveredAt(src);
                    probed.put(object, true);
                }
                tojo.withProbed(count);
                return count;
            }
        ).total();
    }
}
