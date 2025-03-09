/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
import java.util.List;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.eolang.parser.EoSyntax;
import org.w3c.dom.Node;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Parse EO to XML.
 *
 * @since 0.1
 */
@Mojo(
    name = "parse",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class ParseMojo extends SafeMojo {

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
        final int total = new Threaded<>(
            new Filtered<>(
                TjForeign::notParsed,
                this.scopedTojos().withSources()
            ),
            this::parsed
        ).total();
        if (0 == total) {
            if (this.scopedTojos().withSources().isEmpty()) {
                Logger.info(
                    this,
                    "No .eo sources registered, nothing to be parsed to XMIRs (maybe you forgot to execute the \"register\" goal?)"
                );
            } else {
                Logger.info(
                    this,
                    "No new .eo sources out of %d parsed to XMIRs",
                    this.scopedTojos().withSources().size()
                );
            }
        } else {
            Logger.info(
                this, "Parsed %d new .eo sources out of %d to XMIRs in %[ms]s",
                total, this.scopedTojos().withSources().size(),
                System.currentTimeMillis() - start
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
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.ExceptionAsFlowControl"})
    private int parsed(final TjForeign tojo) throws Exception {
        final Path source = tojo.source();
        final String name = tojo.identifier();
        final Path base = this.targetDir.toPath().resolve(ParseMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        final List<Node> refs = new ArrayList<>(1);
        tojo.withXmir(
            new FpDefault(
                src -> {
                    final Node node = this.parsed(src, name);
                    refs.add(node);
                    return new XMLDocument(node).toString();
                },
                this.cache.toPath().resolve(ParseMojo.CACHE),
                this.plugin.getVersion(),
                new TojoHash(tojo),
                base.relativize(target)
            ).apply(source, target)
        ).withVersion(ParseMojo.version(target, refs));
        final List<Xnav> errors = new Xnav(target)
            .element("program")
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
                    error.attribute("line").text().get(),
                    error.text().get()
                );
            }
        }
        return 1;
    }

    /**
     * Source parsed to {@link Node}.
     * @param source Relative source path
     * @param name Name of the EO object
     * @return Parsed EO object as {@link Node}
     * @throws IOException If fails to parse
     */
    private Node parsed(final Path source, final String name) throws IOException {
        final XML xmir = new EoSyntax(name, new InputOf(source)).parsed();
        final Path src = this.sourcesDir.toPath().relativize(source.toAbsolutePath());
        final Node node = new Xembler(
            new Directives().xpath("/program").attr("source",  src)
        ).applyQuietly(xmir.inner());
        Logger.debug(
            ParseMojo.class,
            "Parsed program '%s' from %[file]s:\n %s",
            name, src, xmir
        );
        return node;
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
        if (!parsed.isEmpty()) {
            node = parsed.get(0);
        } else {
            node = new XMLDocument(target).inner();
        }
        return new Xnav(node)
            .element("program")
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
            .map(meta -> meta.element("tail").text().get())
            .orElse(ParseMojo.ZERO);
    }
}
