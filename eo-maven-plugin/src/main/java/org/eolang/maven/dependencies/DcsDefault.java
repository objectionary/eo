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
package org.eolang.maven.dependencies;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;
import org.apache.maven.model.Dependency;
import org.eolang.maven.AssembleMojo;
import org.eolang.maven.Coordinates;
import org.eolang.maven.ParseMojo;
import org.eolang.maven.util.Rel;

/**
 * It is a list of dependencies that are needed by the build.
 *
 * @since 0.29.0
 */
public final class DcsDefault implements Iterable<Dependency> {

    /**
     * List of tojos.
     */
    private final Tojos tojos;

    /**
     * Discover self too.
     */
    private final boolean discover;

    /**
     * Skip if zero version.
     */
    private final boolean skip;

    /**
     * Ctor.
     * @param tjs Tojos
     * @param self Self
     * @param skip Skip
     */
    public DcsDefault(final Tojos tjs, final boolean self,
        final boolean skip) {
        this.tojos = tjs;
        this.discover = self;
        this.skip = skip;
    }

    @Override
    public Iterator<Dependency> iterator() {
        final Collection<Tojo> list = this.tojos.select(
            t -> t.exists(AssembleMojo.ATTR_XMIR)
                && t.exists(AssembleMojo.ATTR_VERSION)
                && !t.exists(AssembleMojo.ATTR_JAR)
        );
        Logger.debug(
            this, "%d suitable tojo(s) found out of %d",
            list.size(), this.tojos.select(t -> true).size()
        );
        final Collection<Dependency> deps = new HashSet<>(0);
        for (final Tojo tojo : list) {
            if (ParseMojo.ZERO.equals(tojo.get(AssembleMojo.ATTR_VERSION))
                && !this.discover) {
                Logger.debug(
                    this, "Program %s/%s skipped due to its zero version",
                    tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION)
                );
                continue;
            }
            final Optional<Dependency> opt = DcsDefault.artifact(
                Paths.get(tojo.get(AssembleMojo.ATTR_XMIR))
            );
            if (!opt.isPresent()) {
                Logger.debug(
                    this, "No dependencies for %s/%s",
                    tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION)
                );
                continue;
            }
            final Dependency dep = opt.get();
            if (this.skip && ParseMojo.ZERO.equals(dep.getVersion())) {
                Logger.debug(
                    this, "Zero-version dependency for %s/%s skipped: %s",
                    tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION),
                    new Coordinates(dep)
                );
                continue;
            }
            Logger.info(
                this, "Dependency found for %s/%s: %s",
                tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION),
                new Coordinates(dep)
            );
            deps.add(dep);
            tojo.set(AssembleMojo.ATTR_JAR, new Coordinates(dep));
        }
        return deps.iterator();
    }

    /**
     * Find the artifact required by this EO XML.
     *
     * @param file EO file
     * @return List of artifact needed
     */
    private static Optional<Dependency> artifact(final Path file) {
        final Collection<String> coords;
        try {
            coords = new XMLDocument(file).xpath(
                "//meta[head='rt' and part[1]='jvm']/part[2]/text()"
            );
        } catch (final FileNotFoundException ex) {
            throw new IllegalStateException(ex);
        }
        final Optional<Dependency> dep;
        if (coords.isEmpty()) {
            dep = Optional.empty();
        } else if (coords.size() == 1) {
            final String[] parts = coords.iterator().next().split(":");
            final Dependency dependency = new Dependency();
            dependency.setGroupId(parts[0]);
            dependency.setArtifactId(parts[1]);
            if (parts.length == 3) {
                dependency.setVersion(parts[2]);
                dependency.setClassifier("");
            } else {
                dependency.setClassifier(parts[2]);
                dependency.setVersion(parts[3]);
            }
            dependency.setScope("transpile");
            dep = Optional.of(dependency);
        } else {
            throw new IllegalStateException(
                String.format(
                    "Too many (%d) dependencies at %s",
                    coords.size(), new Rel(file)
                )
            );
        }
        return dep;
    }

}
