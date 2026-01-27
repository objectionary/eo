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
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.list.ListOf;

/**
 * Go through all `probe` and `also` metas in XMIR files, try to locate the
 * objects pointed by `probe` in Objectionary, and if found, register them in
 * the catalog.
 * More about the purpose of this Mojo is in
 * <a href="https://github.com/objectionary/eo/issues/1323">this issue</a>.
 *
 * <p>
 *     This goal just modifies the "foreign" catalog by adding newly discovered
 *     objects to it. It does not download or pull the sources of these objects,
 *     that is the job of {@link MjPull} goal which usually goes after this one.
 *     This goal does not create any files on the disk either.
 * </p>
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
     * Objectionary.
     * @since 0.50
     * @checkstyle MemberNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Objectionary objectionary = new OyIndexed(
        new OyCached(new OyRemote(this.hash, this.proxies()))
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
                    this.scopedTojos().add(object).withDiscoveredAt(src);
                    probed.put(object, true);
                }
                tojo.withProbed(count);
                return count;
            }
        ).total();
    }
}
