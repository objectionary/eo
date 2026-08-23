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
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.list.ListOf;

/**
 * Goes through all {@code probe} and {@code also} metas in XMIR files,
 * tries to locate the objects pointed by {@code probe} in Objectionary,
 * and if found, registers them in the catalog.
 *
 * <p>When a probed object has no source of its own, the package it belongs
 * to is completed from the remote index, since a package that arrives from
 * the registry has to arrive as a whole. A package that the project's own
 * sources provide is not completed: a sibling that the index still lists,
 * but no local source names, is a leftover of an older release and not a
 * missing member of the local package, so {@link Pulling} must not fetch
 * it.</p>
 *
 * @since 0.61.0
 */
final class Probing implements Step {

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
    Probing(final TjsForeign tjs, final Objectionary obj, final boolean net) {
        this.tojos = tjs;
        this.objectionary = obj;
        this.online = net;
    }

    @Override
    public void exec() throws IOException {
        if (this.online) {
            final Collection<TjForeign> unprobed = this.tojos.unprobed();
            if (unprobed.isEmpty()) {
                this.logEmpty();
            } else {
                this.probe(unprobed);
            }
        } else {
            Logger.info(
                this,
                "No programs were probed because eo.offline flag is TRUE"
            );
        }
    }

    private void probe(final Collection<TjForeign> unprobed) {
        final Map<String, Boolean> probed = new ConcurrentHashMap<>(0);
        if (this.probed(unprobed, probed) == 0) {
            Logger.info(
                this,
                "No probes found in %d programs",
                unprobed.size()
            );
        } else {
            Logger.info(
                this, "Found %d probe(s) in %d program(s): %s",
                probed.size(), unprobed.size(),
                probed.keySet()
            );
        }
    }

    private void logEmpty() {
        if (this.tojos.size() == 0) {
            Logger.warn(this, "Nothing to probe, since there are no programs");
        } else {
            Logger.info(
                this,
                "Nothing to probe, all %d programs checked already",
                this.tojos.size()
            );
        }
    }

    private int probed(
        final Collection<TjForeign> unprobed,
        final Map<String, Boolean> probed) {
        final Set<String> completed = ConcurrentHashMap.newKeySet();
        return new Threaded<>(
            unprobed,
            tojo -> {
                final int count = this.register(tojo.xmir(), probed, completed);
                tojo.withProbed(count);
                return count;
            }
        ).total();
    }

    private int register(
        final Path src,
        final Map<String, Boolean> probed,
        final Set<String> completed
    ) throws IOException {
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
            final TjForeign added = this.tojos.add(object).withDiscoveredAt(src);
            probed.put(object, true);
            if (added.sourceless()) {
                this.complete(object, src, completed);
            }
        }
        return count;
    }

    private void complete(
        final String object,
        final Path src,
        final Set<String> completed
    ) throws IOException {
        final String pkg = object.substring(
            0, Math.max(object.lastIndexOf('.'), 0)
        );
        if (completed.add(pkg)) {
            final String root = "org.eolang.";
            final boolean rooted = object.startsWith(root);
            for (final String sibling : this.objectionary.children(pkg)) {
                final String qualified;
                if (rooted && !sibling.startsWith(root)) {
                    qualified = root.concat(sibling);
                } else {
                    qualified = sibling;
                }
                this.tojos.add(qualified).withDiscoveredAt(src);
            }
        }
    }
}
