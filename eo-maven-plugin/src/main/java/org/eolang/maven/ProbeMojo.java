/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import org.cactoos.iterable.Filtered;
import org.cactoos.iterator.Mapped;
import org.cactoos.list.ListOf;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.ChSafe;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.objectionary.Objectionaries;
import org.eolang.maven.objectionary.Objectionary;
import org.eolang.maven.objectionary.OyFallbackSwap;
import org.eolang.maven.objectionary.OyHome;
import org.eolang.maven.objectionary.OyIndexed;
import org.eolang.maven.objectionary.OyRemote;
import org.eolang.maven.objectionary.OysSimple;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.Rel;

/**
 * Go through all `probe` metas in XMIR files, try to locate the
 * objects pointed by `probe` in Objectionary and if found register them in
 * catalog.
 * More about the purpose of this Mojo is in
 * <a href="https://github.com/objectionary/eo/issues/1323">this issue</a>.
 *
 * @todo #1602:30min Resolve code duplication. Probe and Pull mojos have several
 * identical fields, methods and lines of code. Need to resolve this code
 * duplication. One more abstract class is not an option. We can either join
 * them into one mojo, or composite them inside other mojo.
 * @checkstyle CyclomaticComplexityCheck (300 lines)
 * @since 0.28.11
 */
@Mojo(
    name = "probe",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class ProbeMojo extends SafeMojo {
    /**
     * Default tag.
     */
    private static final String TAG = "master";

    /**
     * Default hash.
     * @checkstyle MemberNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private CommitHash defaultHash;

    /**
     * Objectionaries.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Objectionaries objectionaries = new OysSimple();

    @Override
    public void exec() throws IOException {
        final CommitHash hash = new ChCached(
            new ChSafe(
                this.defaultHash,
                new ChRemote(ProbeMojo.TAG)
            )
        );
        final Collection<String> probed = new HashSet<>(1);
        final Collection<ForeignTojo> tojos = this.scopedTojos().unprobed();
        for (final ForeignTojo tojo : tojos) {
            final Path src = tojo.optimized();
            final Collection<String> names = this.probes(src);
            if (!names.isEmpty()) {
                Logger.info(this, "Probing object(s): %s", names);
            }
            int count = 0;
            for (final String name : names) {
                if (!this.objectionaryByHash(hash).contains(name)) {
                    continue;
                }
                ++count;
                this.scopedTojos()
                    .add(name)
                    .withDiscoveredAt(src);
                probed.add(name);
            }
            tojo.withHash(new ChNarrow(hash)).withProbed(count);
        }
        if (tojos.isEmpty()) {
            if (this.scopedTojos().size() == 0) {
                Logger.warn(this, "Nothing to probe, since there are no programs");
            } else {
                Logger.info(this, "Nothing to probe, all programs checked already");
            }
        } else if (probed.isEmpty()) {
            Logger.debug(
                this, "No probes found in %d programs",
                tojos.size()
            );
        } else {
            Logger.info(
                this, "Found %d probe(s) in %d program(s): %s",
                probed.size(), tojos.size(), probed
            );
        }
    }

    /**
     * Get objectionary by given hash from the map.
     *
     * @param hash Hash.
     * @return Objectionary by given hash.
     */
    private Objectionary objectionaryByHash(final CommitHash hash) {
        return this.objectionaries
            .with(
                hash,
                new OyFallbackSwap(
                    new OyHome(
                        new ChNarrow(hash),
                        this.cache
                    ),
                    new OyIndexed(
                        new OyRemote(hash)
                    ),
                    this::forceUpdate
                )
            )
            .get(hash);
    }

    /**
     * Find all probes found in the provided XML file.
     *
     * @param file The .xmir file
     * @return List of foreign objects found
     * @throws FileNotFoundException If not found
     */
    private Collection<String> probes(final Path file) throws FileNotFoundException {
        final Collection<String> objects = new ListOf<>(
            new Mapped<>(
                ProbeMojo::noPrefix,
                new Filtered<>(
                    obj -> !obj.isEmpty(),
                    new XMLDocument(file).xpath(
                        "//metas/meta[head/text() = 'probe']/tail/text()"
                    )
                ).iterator()
            )
        );
        if (objects.isEmpty()) {
            Logger.debug(
                this, "Didn't find any probed objects in %s",
                new Rel(file)
            );
        } else {
            Logger.debug(
                this, "Found %d probed objects in %s: %s",
                objects.size(), new Rel(file), objects
            );
        }
        return objects;
    }

    /**
     * Trim Q prefix.
     * Q.a.b.c -> a.b
     * a.b.c -> a.b.c
     *
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

    /**
     * Is force update option enabled.
     *
     * @return True if option enabled and false otherwise
     */
    private boolean forceUpdate() {
        return this.session.getRequest().isUpdateSnapshots();
    }
}
