/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;

/**
 * It is a list of dependencies that are needed by the build.
 *
 * @since 0.29.0
 */
final class DcsDefault implements Iterable<Dependency> {

    /**
     * List of tojos.
     */
    private final TjsForeign tojos;

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
    DcsDefault(
        final TjsForeign tjs,
        final boolean self,
        final boolean skip
    ) {
        this.tojos = tjs;
        this.discover = self;
        this.skip = skip;
    }

    @Override
    public Iterator<Dependency> iterator() {
        final Collection<TjForeign> list = this.tojos.dependencies();
        Logger.debug(
            this, "%d suitable tojo(s) found out of %d",
            list.size(), this.tojos.size()
        );
        final Collection<Dependency> deps = new HashSet<>(0);
        for (final TjForeign tojo : list) {
            if (ParseMojo.ZERO.equals(tojo.version()) && !this.discover) {
                Logger.debug(
                    this,
                    "Program %s skipped due to its zero version",
                    tojo.description()
                );
                continue;
            }
            final Optional<Dependency> opt = DcsDefault.artifact(tojo.xmir());
            if (opt.isEmpty()) {
                Logger.debug(this, "No dependencies for %s", tojo.description());
                continue;
            }
            final Dependency dep = opt.get();
            if (this.skip && ParseMojo.ZERO.equals(dep.getVersion())) {
                Logger.debug(
                    this, "Zero-version dependency for %s skipped: %s",
                    tojo.description(),
                    new Coordinates(dep)
                );
                continue;
            }
            Logger.info(
                this, "Dependency found for %s: %s",
                tojo.description(),
                new Coordinates(dep)
            );
            deps.add(dep);
            tojo.withJar(new Coordinates(dep));
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
        final Collection<String> coords = DcsDefault.jvms(file);
        if (coords.size() > 1) {
            throw new IllegalStateException(
                Logger.format("Too many (%d) dependencies at %[file]s", coords.size(), file)
            );
        }
        final Optional<Dependency> dep;
        if (coords.isEmpty()) {
            dep = Optional.empty();
        } else {
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
        }
        return dep;
    }

    /**
     * Return collection of +rt metas.
     * The equivalent xpath is "/program/metas/meta[head='rt' and part[1]='jvm']/part[2]/text()"
     * @param file XML file
     * @return Collection of runtime metas
     */
    private static Collection<String> jvms(final Path file) {
        return new Xnav(file)
            .element("program")
            .elements(Filter.withName("metas"))
            .findFirst()
            .map(
                metas -> metas.elements(
                    Filter.all(
                        Filter.withName("meta"),
                        meta -> {
                            final Xnav xnav = new Xnav(meta);
                            final Optional<String> head = xnav.element("head").text();
                            final boolean runtime = head.map("rt"::equals).orElse(false);
                            final Optional<Xnav> part = xnav.elements(
                                Filter.withName("part")
                            ).findFirst();
                            return runtime
                                && part.isPresent()
                                && part.get().text().map("jvm"::equals).orElse(false);
                        }
                    )
                )
                .map(
                    meta -> meta
                        .elements(Filter.withName("part"))
                        .limit(2)
                        .reduce((first, second) -> second)
                        .get()
                        .text()
                        .get()
                )
                .collect(Collectors.toList())
            ).orElse(List.of());
    }
}
