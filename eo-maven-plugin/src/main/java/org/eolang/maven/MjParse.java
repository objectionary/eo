/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.iterable.Filtered;
import org.cactoos.text.TextOf;
import org.w3c.dom.Node;

/**
 * Parse EO to XML.
 *
 * <p>
 *     This is the initial goal that parses all found EO sources to XMIRs.
 *     You can read more about XMIR format
 *     <a href="https://www.eolang.org/XMIR.html">here</a>
 * </p>
 * <p>
 *    The goal scans all the EO sources registered in the foreign file catalog
 *    (see {@link MjRegister} and {@link MjPull}) and then parses those that were not parsed
 *    before (i.e. do not have XMIRs yet) to XMIR format.
 *    The resulting XMIR files are stored in the {@link #DIR} directory.
 * </p>
 *
 * @since 0.1
 */
@Mojo(
    name = "parse",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class MjParse extends MjSafe {

    /**
     * Zero version.
     */
    static final String ZERO = "0.0.0";

    /**
     * The directory where to parse to.
     */
    static final String DIR = "1-parse";

    /**
     * Subdirectory for parsed cache.
     */
    static final String CACHE = "parsed";

    @Override
    public void exec() {
        final long start = System.currentTimeMillis();
        final Collection<TjForeign> tojos = this.scopedTojos().withSources();
        final int total = new Threaded<>(
            new Filtered<>(TjForeign::notParsed, tojos),
            this::parsed
        ).total();
        if (0 == total) {
            if (tojos.isEmpty()) {
                Logger.info(
                    this,
                    "No .eo sources registered, nothing to be parsed to XMIRs (maybe you forgot to execute the \"register\" goal?)"
                );
            } else {
                Logger.info(
                    this,
                    "No new .eo sources out of %d parsed to XMIRs",
                    tojos.size()
                );
            }
        } else {
            Logger.info(
                this, "Parsed %d new .eo sources out of %d to XMIRs in %[ms]s",
                total, tojos.size(), System.currentTimeMillis() - start
            );
        }
    }

    /**
     * Parse EO file to XML.
     *
     * @param tojo The tojo
     * @return Amount of parsed tojos
     * @throws IOException If fails
     */
    private int parsed(final TjForeign tojo) throws Exception {
        final Path source = tojo.source();
        final String name = tojo.identifier();
        final Path base = this.targetDir.toPath().resolve(MjParse.DIR);
        final Path target = new Place(name).make(base, MjAssemble.XMIR);
        final List<Node> refs = new ArrayList<>(1);
        if (this.cacheEnabled) {
            new ConcurrentCache(
                new Cache(
                    new CachePath(
                        this.cache.toPath().resolve(MjParse.CACHE),
                        this.plugin.getVersion(),
                        new TojoHash(tojo).get()
                    ),
                    src -> {
                        final Node node = this.parsed(src, name);
                        refs.add(node);
                        return new XMLDocument(node).toString();
                    }
                )
            ).apply(source, target, base.relativize(target));
        } else {
            refs.add(this.parsed(source, name));
        }
        tojo.withXmir(target).withVersion(MjParse.version(target, refs));
        final List<Xnav> errors = new Xnav(target)
            .element("object")
            .element("errors")
            .elements(Filter.withName("error"))
            .collect(Collectors.toList());
        if (errors.isEmpty()) {
            Logger.debug(this, "Parsed %[file]s to %[file]s", source, target);
        } else {
            for (final Xnav error : errors) {
                Logger.error(
                    this,
                    "Failed to parse '%[file]s:%s': %s",
                    source,
                    error.attribute("line").text().orElse("0"),
                    error.text().orElse("")
                );
            }
        }
        return 1;
    }

    /**
     * Source parsed to {@link Node}.
     * @param source Relative source path
     * @param identifier Name of the EO object as tojo identifier
     * @return Parsed EO object as {@link Node}
     * @throws IOException If fails to parse
     */
    private Node parsed(final Path source, final String identifier) throws IOException {
        final EoSource.Xmir xmir = new EoSource(identifier, source).parsed();
        Logger.debug(
            MjParse.class,
            "Parsed program '%s' from %[file]s:\n %s",
            identifier, this.sourcesDir.toPath().relativize(source.toAbsolutePath()), xmir
        );
        if (xmir.broken()) {
            new Saved(
                new TextOf(xmir.xml().toString()),
                this.targetDir.toPath().resolve(
                    String.format("broken-%x.xmir", System.nanoTime())
                )
            ).value();
        }
        return xmir.xml().inner();
    }

    /**
     * Tojo version.
     * The version can be extracted from:
     * 1. Parsed {@link Node} if EO object was parsed for the first time
     * 2. XML document that was already parsed before
     * @param target Path to result XML document
     * @param parsed List with either one parsed {@link Node} or empty
     * @return Tojo version
     * @throws FileNotFoundException If XML document file does not exist
     */
    private static String version(
        final Path target,
        final List<Node> parsed
    ) throws FileNotFoundException {
        final Node node;
        if (parsed.isEmpty()) {
            node = new XMLDocument(target).inner();
        } else {
            node = parsed.get(0);
        }
        return new Xnav(node)
            .element("object")
            .element("metas")
            .elements(
                Filter.all(
                    Filter.withName("meta"),
                    meta -> new Xnav(meta)
                        .elements(
                            Filter.all(
                                Filter.withName("head"),
                                head -> head.text().map("version"::equals).orElse(false)
                            )
                        )
                        .findAny()
                        .isPresent()
                )
            )
            .findFirst()
            .map(meta -> meta.element("tail").text().orElse(MjParse.ZERO))
            .orElse(MjParse.ZERO);
    }
}
