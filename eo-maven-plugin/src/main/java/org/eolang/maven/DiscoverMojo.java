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
import com.jcabi.xml.SaxonDocument;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.iterable.Filtered;
import org.cactoos.list.ListOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.Rel;

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
    public void exec() throws FileNotFoundException {
        final Collection<ForeignTojo> tojos = this.scopedTojos().notDiscovered();
        final Collection<String> discovered = new HashSet<>(1);
        for (final ForeignTojo tojo : tojos) {
            final Path src = tojo.optimized();
            tojo.withDiscovered(
                (int) this.discover(src)
                    .stream()
                    .filter(name -> !name.isEmpty())
                    .peek(name -> this.scopedTojos().add(name).withDiscoveredAt(src))
                    .peek(discovered::add)
                    .count()
            );
        }
        if (tojos.isEmpty()) {
            if (this.scopedTojos().size() == 0) {
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
        final XML saxon = new SaxonDocument(file);
        final Collection<String> names = DiscoverMojo.names(saxon, this.xpath(false));
        if (this.withVersions) {
            names.addAll(
                DiscoverMojo.names(saxon, this.xpath(true))
            );
        }
        if (!new XMLDocument(file).nodes("//o[@vararg]").isEmpty()) {
            names.add("org.eolang.tuple");
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

    /**
     * Xpath for selecting objects from given xml.
     * @param versioned Select with versions or not.
     * @return Xpath as list of strings
     * @todo #1602:30min Simplify xpath. Current implementation for building
     *  xpath with and without versions is quite ugly. For some reason
     *  if we try to take `/concat(@base,'|',@ver)` and there are object without
     *  attribute `ver` - xpath returns nothing. So we need to take `/@base`
     *  from objects where attribute `ver` is not present in both cases and
     *  then if flag `withVersions` is `true` - take `concat(@base,'|',@ver)`
     *  from objects attribute `ver` is present.
     */
    private String xpath(final boolean versioned) {
        final Collection<String> xpath = new ListOf<>(
            "//o[",
            "not(starts-with(@base,'.'))",
            "and @base != 'Q'",
            "and @base != '^'",
            "and @base != '$'",
            "and @base != '&'",
            "and not(@ref)"
        );
        final List<String> tail;
        if (versioned) {
            tail = new ListOf<>(
                "and @ver",
                "]/concat(@base,'|',@ver)"
            );
        } else {
            tail = new ListOf<>();
            if (this.withVersions) {
                tail.add("and not(@ver)");
            }
            tail.add("]/@base");
        }
        xpath.addAll(tail);
        return String.join(
            " ",
            xpath
        );
    }

    /**
     * Get unique list of object names from given XML by provided xpath.
     * @param xml XML.
     * @param xpath Xpath.
     * @return Iterable of object names.
     */
    private static Set<String> names(final XML xml, final String xpath) {
        return new SetOf<>(
            new Filtered<>(
                obj -> !obj.isEmpty(),
                xml.xpath(xpath)
            )
        );
    }
}
