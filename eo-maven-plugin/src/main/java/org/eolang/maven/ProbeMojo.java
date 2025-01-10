/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.tojos.ForeignTojo;

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
     * The Git tag to pull objects from, in objectionary.
     *
     * @since 0.21.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.tag", required = true, defaultValue = "master")
    private String tag = "master";

    /**
     * The Git hash to pull objects from, in objectionary.
     * If not set, will be computed from {@code tag} field.
     * @since 0.29.6
     */
    @SuppressWarnings("PMD.ImmutableField")
    private CommitHash hash = new ChCached(
        new ChNarrow(
            new ChRemote(this.tag)
        )
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
    private void probe() throws IOException {
        final long start = System.currentTimeMillis();
        final Collection<String> probed = new HashSet<>(0);
        final Collection<ForeignTojo> tojos = this.scopedTojos().unprobed();
        for (final ForeignTojo tojo : tojos) {
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
                this.scopedTojos()
                    .add(object)
                    .withDiscoveredAt(src);
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
            Logger.debug(
                this, "No probes found in %d programs",
                tojos.size()
            );
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
     * @throws FileNotFoundException If not found
     */
    private Collection<String> probes(final Path file) throws FileNotFoundException {
        final long start = System.currentTimeMillis();
        final Collection<String> objects = new ListOf<>(
            new Mapped<>(
                ProbeMojo::noPrefix,
                new Filtered<>(
                    obj -> !obj.isEmpty(),
                    new XMLDocument(file).xpath(
                        "/program/metas/meta[head/text() = 'probe']/tail/text()"
                    )
                )
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
