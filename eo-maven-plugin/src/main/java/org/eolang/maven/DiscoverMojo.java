/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
import com.jcabi.xml.SaxonDocument;
import com.jcabi.xml.XML;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.iterable.Filtered;
import org.cactoos.set.SetOf;
import org.eolang.maven.tojos.ForeignTojo;

/**
 * Read all XMIR files and find foreign objects in them, then
 * add them to the catalog.
 *
 * @since 0.1
 */
@Mojo(
    name = "discover-foreign",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class DiscoverMojo extends SafeMojo {
    /**
     * Xpath for discovering.
     */
    private static final String XPATH = String.join(
        "",
        "//o[",
        "not(starts-with(@base, '.'))",
        " and @base != 'Q'",
        " and @base != '^'",
        " and @base != '$'",
        " and @base != '∅'",
        " and not(@ref)",
        "]/@base"
    );

    @Override
    public void exec() {
        final Collection<ForeignTojo> tojos = this.scopedTojos().notDiscovered();
        final Collection<String> discovered = new HashSet<>();
        for (final ForeignTojo tojo : tojos) {
            final Path src = tojo.shaken();
            final Collection<String> names = this.discover(src);
            discovered.addAll(names);
            for (final String name : names) {
                this.scopedTojos().add(name).withDiscoveredAt(src);
            }
            tojo.withDiscovered(discovered.size());
        }
        if (tojos.isEmpty()) {
            if (this.scopedTojos().size() == 0) {
                Logger.warn(this, "Nothing to discover, since there are no programs");
            } else {
                Logger.info(
                    this,
                    "Nothing to discover, all %d programs checked already",
                    this.scopedTojos().size()
                );
            }
        } else if (discovered.isEmpty()) {
            Logger.info(
                this, "No foreign objects discovered in %d programs",
                tojos.size()
            );
        } else {
            Logger.info(
                this, "Discovered %d foreign objects in %d programs: %s",
                discovered.size(), tojos.size(), discovered
            );
        }
    }

    /**
     * Pull all deps found in the provided XML file.
     *
     * @param file The .xmir file
     * @return List of foreign objects found
     */
    private Collection<String> discover(final Path file) {
        final XML xml = new SaxonDocument(file);
        final Collection<String> names = DiscoverMojo.names(xml);
        if (names.isEmpty()) {
            Logger.debug(
                this, "Didn't find any foreign objects in %[file]s",
                file
            );
        } else {
            Logger.debug(
                this, "Found %d foreign objects in %[file]s: %s",
                names.size(), file, names
            );
        }
        return names;
    }

    /**
     * Get a unique list of object names from given XML.
     *
     * @param xml XML.
     * @return Object names.
     */
    private static Collection<String> names(final XML xml) {
        return new SetOf<>(
            new Filtered<>(
                name -> !name.isEmpty(),
                xml.xpath(DiscoverMojo.XPATH)
            )
        );
    }
}
