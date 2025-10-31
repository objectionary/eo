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
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
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
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.list.ListOf;
import org.cactoos.text.TextOf;
import org.eolang.parser.EoSyntax;
import org.eolang.parser.OnDefault;
import org.eolang.parser.OnDetailed;
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
public final class MjParse extends MjSafe {
    /**
     * Reserved object bases from QQ.
     */
    private static final String QQS = String.join(
        ",",
        new ListOf<>(
            "Φ.org.eolang.number",
            "Φ.org.eolang.bytes"
        )
    );

    /**
     * Optimization line.
     */
    private static final Xsline OPTIMIZATION_LINE = new Xsline(
        new TrDefault<>(
            new StClasspath(
                "/org/eolang/maven/parse/locals-to-aliases.xsl",
                "name reserved",
                String.format(
                    "reserved %s",
                    MjParse.QQS
                )
            )
        )
    );

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
        tojo.withXmir(
            new FpDefault(
                src -> {
                    final Node node = this.parsed(src, name);
                    refs.add(node);
                    return MjParse.OPTIMIZATION_LINE.pass(new XMLDocument(node)).toString();
                },
                this.cache.toPath().resolve(MjParse.CACHE),
                this.plugin.getVersion(),
                new TojoHash(tojo),
                base.relativize(target),
                this.cacheEnabled
            ).apply(source, target)
        ).withVersion(MjParse.version(target, refs));
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
        final XML xmir = new EoSyntax(new InputOf(source)).parsed();
        Logger.debug(
            MjParse.class,
            "Parsed program '%s' from %[file]s:\n %s",
            identifier, this.sourcesDir.toPath().relativize(source.toAbsolutePath()), xmir
        );
        final Node document = xmir.inner();
        final String name = new OnDetailed(
            new OnDefault(xmir),
            e -> MjParse.applyError("mandatory-object-name", e.getMessage(), document)
        ).get();
        if (!name.equals(identifier)) {
            MjParse.applyError(
                "validate-object-name",
                Logger.format(
                    "For some reason, the identifier of the tojo, which essentially is a name of the source file ('%s'), does not match the name of the object discovered in the XMIR after parsing ('%s'); the XMIR is saved to the %[file]s file, for debugging purposes",
                    identifier, name,
                    new Saved(
                        new TextOf(xmir.toString()),
                        this.targetDir.toPath().resolve(
                            String.format("broken-%x.xmir", System.nanoTime())
                        )
                    ).value()
                ),
                document
            );
        }
        return document;
    }

    /**
     * Apply error to the document.
     * @param check Check name
     * @param message Error message
     * @param document Document
     */
    private static void applyError(
        final String check, final String message, final Node document
    ) {
        new Xembler(
            new Directives()
                .xpath("/object")
                .addIf("errors")
                .add("error")
                .attr("check", check)
                .attr("severity", "critical")
                .set(message)
        ).applyQuietly(document);
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
        final Path target, final List<Node> parsed
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
