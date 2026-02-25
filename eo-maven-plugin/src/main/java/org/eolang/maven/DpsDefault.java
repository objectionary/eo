/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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

/**
 * List of default dependencies.
 * <p>The dependencies are:
 * 1. net.java.dev.jna:jna:5.14.0 which is we need for syscalls
 * 2. all the dependencies extracted from "+rt" metas from XMIRs noted in all the tojos</p>
 * @since 0.29.0
 */
final class DpsDefault implements Dependencies {
    /**
     * JNA dependency.
     */
    private static final Dep JNA = new Dep()
        .withGroupId("net.java.dev.jna")
        .withArtifactId("jna")
        .withVersion("5.14.0");

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
     * Add default JNA dependency or not.
     */
    private final boolean jna;

    /**
     * Ctor.
     * @param tjs Tojos
     * @param self Self
     * @param skip Skip
     * @param jna Add JNA dependency
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    DpsDefault(final TjsForeign tjs, final boolean self, final boolean skip, final boolean jna) {
        this.tojos = tjs;
        this.discover = self;
        this.skip = skip;
        this.jna = jna;
    }

    @Override
    public Iterator<Dep> iterator() {
        final Collection<TjForeign> list = this.tojos.dependencies();
        Logger.debug(
            this, "%d suitable tojo(s) found out of %d",
            list.size(), this.tojos.size()
        );
        final Collection<Dep> deps = new HashSet<>(0);
        if (this.jna) {
            deps.add(DpsDefault.JNA);
        }
        for (final TjForeign tojo : list) {
            if (MjParse.ZERO.equals(tojo.version()) && !this.discover) {
                Logger.debug(
                    this,
                    "Program %s skipped due to its zero version",
                    tojo.description()
                );
                continue;
            }
            final Optional<Dep> opt = DpsDefault.artifact(tojo.xmir());
            if (opt.isEmpty()) {
                Logger.debug(this, "No dependencies for %s", tojo.description());
                continue;
            }
            final Dep dep = opt.get();
            final String coords = dep.toString();
            if (this.skip && MjParse.ZERO.equals(dep.get().getVersion())) {
                Logger.debug(
                    this, "Zero-version dependency for %s skipped: %s",
                    tojo.description(), coords
                );
                continue;
            }
            Logger.info(
                this, "Dependency found for %s: %s",
                tojo.description(), coords
            );
            deps.add(dep);
            tojo.withJar(coords);
        }
        return deps.iterator();
    }

    /**
     * Find the artifact required by this EO XML.
     *
     * @param file EO file
     * @return List of artifact needed
     */
    private static Optional<Dep> artifact(final Path file) {
        final Collection<String> coords = DpsDefault.jvms(file);
        if (coords.size() > 1) {
            throw new IllegalStateException(
                Logger.format("Too many (%d) dependencies at %[file]s", coords.size(), file)
            );
        }
        final Optional<Dep> dep;
        if (coords.isEmpty()) {
            dep = Optional.empty();
        } else {
            final String[] parts = coords.iterator().next().split(":");
            final Dep dependency = new Dep().withGroupId(parts[0]).withArtifactId(parts[1]);
            if (parts.length == 3) {
                dependency.withClassifier("").withVersion(parts[2]);
            } else {
                dependency.withClassifier(parts[2]).withVersion(parts[3]);
            }
            dep = Optional.of(dependency.withScope("transpile"));
        }
        return dep;
    }

    /**
     * Return collection of +rt metas.
     * The equivalent xpath is "/object/metas/meta[head='rt' and part[1]='jvm']/part[2]/text()"
     * @param file XML file
     * @return Collection of runtime metas
     */
    private static Collection<String> jvms(final Path file) {
        return new Xnav(file)
            .element("object")
            .elements(Filter.withName("metas"))
            .findFirst()
            .map(
                metas -> metas.elements(
                    Filter.all(
                        Filter.withName("meta"),
                        meta -> {
                            final Xnav xnav = new Xnav(meta);
                            final Optional<Xnav> part = xnav.elements(
                                Filter.withName("part")
                            ).findFirst();
                            return xnav.element("head").text().map("rt"::equals).orElse(false)
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
