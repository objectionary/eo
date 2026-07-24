/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.text.HexOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.Canonical;
import org.w3c.dom.Node;

/**
 * Parse EO to XML.
 *
 * <p>
 *     This class parses all found EO sources to XMIRs.
 *     You can read more about XMIR format
 *     <a href="https://www.eolang.org/XMIR.html">here</a>
 * </p>
 * <p>
 *    The class scans all the EO sources registered in the foreign file catalog
 *    and then parses those that were not parsed before (i.e. do not have XMIRs yet)
 *    to XMIR format. The resulting XMIR files are stored in the {@link #DIR} directory.
 * </p>
 *
 * @since 0.1
 */
final class Parsing implements Step {

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

    /**
     * Foreign tojos catalog.
     */
    private final TjsForeign tojos;

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Path targetDir;

    /**
     * Base cache directory.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Path cacheDir;

    /**
     * Whether caching is enabled.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final boolean cacheEnabled;

    /**
     * Plugin version.
     */
    private final String version;

    /**
     * EO sources directory (used for logging).
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Path sourcesDir;

    /**
     * Shared cache guard, reused across all files so that concurrent writes to
     * the same cache tail path are actually serialized (#5720).
     */
    private final ConcurrentCache guard;

    /**
     * Constructor.
     * @param srcs Foreign tojos catalog
     * @param target Target directory
     * @param cache Base cache directory
     * @param enabled Whether caching is enabled
     * @param ver Plugin version string
     * @param sources EO sources directory
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Parsing(
        final TjsForeign srcs,
        final Path target,
        final Path cache,
        final boolean enabled,
        final String ver,
        final Path sources
    ) {
        this.tojos = srcs;
        this.targetDir = target;
        this.cacheDir = cache;
        this.cacheEnabled = enabled;
        this.version = ver;
        this.sourcesDir = sources;
        this.guard = new ConcurrentCache();
    }

    @Override
    public void exec() {
        final Collection<TjForeign> sources = this.tojos.withSources();
        final String objects = sources.stream()
            .map(TjForeign::identifier)
            .filter(id -> id.contains("."))
            .distinct()
            .sorted()
            .collect(Collectors.joining(" "));
        final int total = this.parsed(
            sources,
            new Canonical(objects),
            new UncheckedText(
                new HexOf(new Sha256DigestOf(new InputOf(objects)))
            ).asString()
        );
        if (0 == total) {
            if (sources.isEmpty()) {
                Logger.info(
                    this,
                    "No .eo sources registered, nothing to be parsed to XMIRs (maybe you forgot to execute the \"register\" goal?)"
                );
            } else {
                Logger.info(
                    this,
                    "No new .eo sources out of %d parsed to XMIRs",
                    sources.size()
                );
            }
        } else {
            Logger.info(
                this, "Parsed %d new .eo sources out of %d to XMIRs",
                total, sources.size()
            );
        }
    }

    /**
     * Parse all the given sources to XMIRs, concurrently.
     * @param sources The sources to parse
     * @param pipeline The canonical parsing transform to apply
     * @param digest Digest of the set of known objects (part of the cache key)
     * @return Amount of parsed tojos
     */
    private int parsed(
        final Collection<TjForeign> sources,
        final UnaryOperator<XML> pipeline,
        final String digest
    ) {
        return new Threaded<>(
            new Filtered<>(TjForeign::notParsed, sources),
            tojo -> this.parsed(tojo, pipeline, digest)
        ).total();
    }

    /**
     * Parse EO file to XML.
     * @param tojo The tojo
     * @param pipeline The canonical parsing transform to apply
     * @param digest Digest of the set of known objects (part of the cache key)
     * @return Amount of parsed tojos
     * @throws Exception If fails
     */
    private int parsed(
        final TjForeign tojo, final UnaryOperator<XML> pipeline, final String digest
    ) throws Exception {
        final Path source = tojo.source();
        final String name = tojo.identifier();
        final Path base = this.targetDir.resolve(Parsing.DIR);
        final Path target = new Place(name).make(base, MjAssemble.XMIR);
        final List<Node> refs = new ArrayList<>(1);
        if (this.cacheEnabled) {
            this.guard.apply(
                new Cache(
                    new CachePath(
                        this.cacheDir.resolve(Parsing.CACHE),
                        String.format("%s-%s", this.version, digest),
                        new TojoHash(tojo).get()
                    ),
                    src -> {
                        final Node node = this.parsed(src, name, pipeline);
                        refs.add(node);
                        return new XMLDocument(node).toString();
                    }
                ),
                source, target, base.relativize(target)
            );
        } else {
            final Node node = this.parsed(source, name, pipeline);
            new Saved(new XMLDocument(node).toString(), target).value();
            refs.add(node);
        }
        tojo.withXmir(target).withVersion(Parsing.tojoVersion(target, refs));
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
     * @param pipeline The canonical parsing transform to apply
     * @return Parsed EO object as {@link Node}
     * @throws IOException If fails to parse
     */
    private Node parsed(
        final Path source, final String identifier, final UnaryOperator<XML> pipeline
    ) throws IOException {
        final EoSource.Xmir xmir = new EoSource(identifier, source, pipeline).parsed();
        Logger.debug(
            Parsing.class,
            "Parsed program '%s' from %[file]s:%n %s",
            identifier, this.sourcesDir.relativize(source.toAbsolutePath()), xmir
        );
        if (xmir.broken()) {
            new Saved(
                new TextOf(xmir.xml().toString()),
                this.targetDir.resolve(
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
    private static String tojoVersion(
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
            .element("metas").elements(
                Filter.all(
                    Filter.withName("meta"),
                    meta -> new Xnav(meta).elements(
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
            .map(meta -> meta.element("tail").text().orElse(Parsing.ZERO))
            .orElse(Parsing.ZERO);
    }
}
