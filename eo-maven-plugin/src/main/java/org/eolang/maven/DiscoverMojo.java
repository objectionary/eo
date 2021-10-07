/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashSet;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.iterable.Filtered;
import org.cactoos.list.ListOf;
import org.cactoos.list.Mapped;
import org.eolang.tojos.MonoTojos;
import org.eolang.tojos.Tojos;

/**
 * Read all XMIR files and find foreign objects in them, then
 * add them to the catalog.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "discover-foreign",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class DiscoverMojo extends SafeMojo {

    @Override
    public void exec() throws IOException {
        final Tojos tojos = new MonoTojos(this.foreign);
        final Collection<Path> sources = new Mapped<>(
            row -> Paths.get(row.get(AssembleMojo.ATTR_XMIR2)),
            tojos.select(row -> row.exists(AssembleMojo.ATTR_XMIR2))
        );
        final Collection<String> discovered = new HashSet<>(1);
        for (final Path source : sources) {
            for (final String name : this.discover(source)) {
                tojos.add(name).set(AssembleMojo.ATTR_VERSION, "*.*.*");
                discovered.add(name);
            }
        }
        if (discovered.isEmpty()) {
            Logger.info(
                this, "No objects discovered in %d sources",
                sources.size()
            );
        } else {
            Logger.info(
                this, "Discovered %d objects in %d sources: %s",
                discovered.size(), sources.size(), discovered
            );
        }
    }

    /**
     * Pull all deps found in the provided XML file.
     *
     * @param file The .eo.xml file
     * @return List of foreign objects found
     * @throws FileNotFoundException If not found
     */
    private Collection<String> discover(final Path file)
        throws FileNotFoundException {
        final XML xml = new XMLDocument(file);
        final Collection<String> names = new HashSet<>(
            new ListOf<>(
                new Filtered<>(
                    obj -> !obj.isEmpty(),
                    xml.xpath(
                        String.join(
                            " ",
                            "//o[",
                            "not(starts-with(@base,'.'))",
                            " and @base != '^'",
                            " and @base != '$'",
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
            Logger.info(
                this, "Didn't find any foreign objects in %s",
                file
            );
        } else {
            Logger.info(
                this, "Found %d foreign objects in %s: %s",
                names.size(), file, names
            );
        }
        return names;
    }

}
