/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashSet;
import java.util.TreeSet;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.iterable.Filtered;
import org.cactoos.list.ListOf;

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

    @Override
    public void exec() throws IOException {
        final Collection<Tojo> tojos = this.scopedTojos().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
                && !row.exists(AssembleMojo.ATTR_DISCOVERED)
        );
        final Collection<String> discovered = new HashSet<>(1);
        for (final Tojo tojo : tojos) {
            final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2));
            final Collection<String> names = this.discover(src);
            for (final String name : names) {
                final Tojo ftojo = this.scopedTojos().add(name);
                if (!ftojo.exists(AssembleMojo.ATTR_VERSION)) {
                    ftojo.set(AssembleMojo.ATTR_VERSION, "*.*.*");
                }
                ftojo.set(AssembleMojo.ATTR_DISCOVERED_AT, src);
                discovered.add(name);
            }
            tojo.set(AssembleMojo.ATTR_DISCOVERED, Integer.toString(names.size()));
        }
        if (tojos.isEmpty()) {
            if (this.scopedTojos().select(row -> true).isEmpty()) {
                Logger.warn(this, "Nothing to discover, since there are no programs");
            } else {
                Logger.info(this, "Nothing to discover, all programs checked already");
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
     * @throws FileNotFoundException If not found
     */
    private Collection<String> discover(final Path file)
        throws FileNotFoundException {
        final XML xml = new XMLDocument(file);
        final Collection<String> names = new TreeSet<>(
            new ListOf<>(
                new Filtered<>(
                    obj -> !obj.isEmpty(),
                    xml.xpath(
                        String.join(
                            " ",
                            "//o[",
                            "not(starts-with(@base,'.'))",
                            " and @base != 'Q'",
                            " and @base != '^'",
                            " and @base != '$'",
                            " and @base != '&'",
                            " and not(@ref)",
                            "]/@base"
                        )
                    )
                )
            )
        );
        if (!xml.nodes("//o[@vararg]").isEmpty()) {
            names.add("org.eolang.array");
        }
        if (names.isEmpty()) {
            Logger.debug(
                this, "Didn't find any foreign objects in %s",
                new Rel(file)
            );
        } else {
            Logger.debug(
                this, "Found %d foreign objects in %s: %s",
                names.size(), new Rel(file), names
            );
        }
        return names;
    }

}
