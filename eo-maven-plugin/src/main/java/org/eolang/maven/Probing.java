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
 * it. An object whose name carries no dot belongs to the root package, which
 * is completed the same way as any other.</p>
 *
 * <p>Every object that arrives this way brings the objects of its own
 * package too, all the way down, such as {@code i64.div} for {@code i64}.
 * The source never names {@code Φ.i64.div}: it writes {@code x.div}, and
 * the call reaches the object only when Java runs it, through the
 * fallback of the dispatch to the package (#5486). Without it, the
 * {@code infer} goal can't tell which void each argument of such a call
 * lands in, and {@code dealpha} leaves it positional (#10083).</p>
 *
 * @since 0.61.0
 */
final class Probing implements Step {

    /**
     * The prefix of the names of the root package.
     */
    private static final String ROOT = "org.eolang.";

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
     *
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
        this.fill(
            object.substring(0, Math.max(object.lastIndexOf('.'), 0)),
            object.startsWith(Probing.ROOT), src, completed
        );
        this.fill(object, object.startsWith(Probing.ROOT), src, completed);
    }

    private void fill(
        final String pkg,
        final boolean rooted,
        final Path src,
        final Set<String> completed
    ) throws IOException {
        if (completed.add(pkg)) {
            for (final String member : this.objectionary.children(pkg)) {
                final String qualified;
                if (rooted && !member.startsWith(Probing.ROOT)) {
                    qualified = Probing.ROOT.concat(member);
                } else {
                    qualified = member;
                }
                if (this.tojos.add(qualified).withDiscoveredAt(src).sourceless()) {
                    this.fill(qualified, rooted, src, completed);
                }
            }
        }
    }
}
