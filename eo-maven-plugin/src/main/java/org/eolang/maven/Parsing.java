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
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import io.github.sekator778.seqdir.Seqdir;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.text.HexOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.cache.GlobalCache;
import org.eolang.cache.Saved;
import org.eolang.cache.Sha;
import org.eolang.parser.Canonical;
import org.w3c.dom.Node;

/**
 * Parse EO to XML.
 *
 * <p>This class parses all found EO sources to XMIRs.
 * You can read more about XMIR format
 * <a href="https://www.eolang.org/XMIR.html">here</a></p>
 *
 * <p>The class scans all the EO sources registered in the foreign file catalog
 * and then parses those that were not parsed before (i.e. do not have XMIRs yet)
 * to XMIR format. The resulting XMIR files are stored in a directory
 * {@code Seqdir} numbers "parse".</p>
 *
 * @since 0.1
 */
final class Parsing implements Step {

    /**
     * Zero version.
     */
    static final String ZERO = "0.0.0";

    /**
     * Directory for parsed cache.
     */
    static final String CACHE = "parsed";

    /**
     * The stylesheet that drops every unit test from an XMIR.
     */
    private static final String DROP = "/org/eolang/maven/parse/drop-tests.xsl";

    /**
     * The shift that drops every unit test from an XMIR, one per thread,
     * since {@link StClasspath} compiles its stylesheet lazily and
     * that is not thread-safe (see {@link Transpilation}).
     */
    private static final ThreadLocal<Shift> UNTESTED =
        ThreadLocal.withInitial(() -> new StClasspath(Parsing.DROP));

    /**
     * Foreign tojos catalog.
     */
    private final TjsForeign tojos;

    /**
     * Target directory.
     */
    private final Path target;

    /**
     * EO sources directory (used for logging).
     */
    private final Path home;

    /**
     * Where the results of earlier builds are looked for and kept.
     */
    private final GlobalCache cache;

    /**
     * Whether the unit tests stay in the XMIR.
     */
    private final boolean tests;

    /**
     * Constructor, which keeps the unit tests in the XMIR.
     *
     * @param srcs Foreign tojos catalog
     * @param target Target directory
     * @param sources EO sources directory
     * @param store Where the results of earlier builds are looked for and kept
     */
    Parsing(
        final TjsForeign srcs,
        final Path target,
        final Path sources,
        final GlobalCache store
    ) {
        this(srcs, target, sources, store, true);
    }

    /**
     * Constructor.
     *
     * @param srcs Foreign tojos catalog
     * @param target Target directory
     * @param sources EO sources directory
     * @param store Where the results of earlier builds are looked for and kept
     * @param tests Whether the unit tests stay in the XMIR
     */
    Parsing(
        final TjsForeign srcs,
        final Path target,
        final Path sources,
        final GlobalCache store,
        final boolean tests
    ) {
        this.tojos = srcs;
        this.target = target;
        this.home = sources;
        this.cache = store;
        this.tests = tests;
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
        final Canonical canonical = new Canonical(objects);
        final UnaryOperator<XML> pipeline;
        final GlobalCache store;
        if (this.tests) {
            pipeline = canonical;
            store = this.cache;
        } else {
            pipeline = xml -> canonical.apply(Parsing.UNTESTED.get().apply(0, xml));
            store = this.cache.with(new Fingerprint(Parsing.DROP).get());
        }
        final int total = this.parsed(
            sources,
            pipeline,
            store.with(
                new Fingerprint(
                    Stream.concat(
                        Canonical.XSLS.stream(), Canonical.IMPORTS.stream()
                    ).toArray(String[]::new)
                ).get()
            ).with(
                new UncheckedText(
                    new HexOf(new Sha256DigestOf(new InputOf(objects)))
                ).asString()
            )
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

    private int parsed(
        final Collection<TjForeign> sources,
        final UnaryOperator<XML> pipeline,
        final GlobalCache store
    ) {
        return new Threaded<>(
            new Filtered<>(this::unparsed, sources),
            tojo -> this.parsed(tojo, pipeline, store)
        ).total();
    }

    private boolean unparsed(final TjForeign tojo) throws IOException {
        return tojo.notParsed()
            || !tojo.xmir().startsWith(
                new Seqdir(this.target, 2).dirs().once("parse")
            );
    }

    private int parsed(
        final TjForeign tojo, final UnaryOperator<XML> pipeline, final GlobalCache store
    ) throws Exception {
        final Path source = tojo.source();
        final String name = tojo.identifier();
        final Path base = new Seqdir(this.target, 2).dirs().once("parse");
        final Path xmir = new Place(name).make(base, MjAssemble.XMIR);
        final List<Node> refs = new ArrayList<>(1);
        store.footprint(
            base.relativize(xmir),
            new TojoHash(tojo),
            src -> {
                final Node node = this.parsed(tojo, pipeline);
                refs.add(node);
                return new XMLDocument(node).toString();
            }
        ).apply(source, xmir);
        tojo.withXmir(xmir)
            .withVersion(Parsing.tojoVersion(xmir, refs))
            .withDigest(new Sha(source).toString());
        final List<Xnav> errors = new Xnav(xmir)
            .element("object")
            .element("errors")
            .elements(Filter.withName("error"))
            .collect(Collectors.toList());
        if (errors.isEmpty()) {
            Logger.debug(this, "Parsed %[file]s to %[file]s", source, xmir);
        } else {
            for (final Xnav error : errors) {
                this.logged(source, error);
            }
        }
        return 1;
    }

    private void logged(final Path source, final Xnav error) {
        final String location = Logger.format(
            "'%[file]s:%s': %s",
            source,
            error.attribute("line").text().orElse("0"),
            error.text().orElse("")
        );
        if ("warning".equals(error.attribute("severity").text().orElse("error"))) {
            Logger.warn(this, "Warning while parsing %s", location);
        } else {
            Logger.error(this, "Failed to parse %s", location);
        }
    }

    private Node parsed(
        final TjForeign tojo, final UnaryOperator<XML> pipeline
    ) throws IOException {
        final Xmir xmir = new EoSource(tojo.identifier(), tojo.source(), pipeline).parsed(
            pipeline.apply(
                new Raws(
                    this.cache.with("raws"), new Seqdir(this.target, 2).dirs().once("raw")
                ).of(tojo)
            )
        );
        Logger.debug(
            Parsing.class,
            "Parsed program '%s' from %[file]s:%n %s",
            tojo.identifier(), this.home.relativize(tojo.source().toAbsolutePath()), xmir
        );
        if (xmir.broken()) {
            new Saved(
                new TextOf(xmir.xml().toString()),
                this.target.resolve(
                    String.format("broken-%x.xmir", System.nanoTime())
                )
            ).value();
        }
        return xmir.xml().inner();
    }

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
