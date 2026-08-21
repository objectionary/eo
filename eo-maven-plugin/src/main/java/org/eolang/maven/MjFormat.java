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
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.EoSyntax;
import org.eolang.printer.Xmir;

/**
 * Enforce the canonical layout of {@code .eo} sources.
 *
 * <p>This goal walks through all registered {@code .eo} sources (see
 * {@link MjRegister}), reformats every one of them in memory by parsing
 * it to XMIR and printing it back with {@link Xmir#toEO()} until the text
 * stops changing (the moniker merge of #5739 is not a single-pass
 * fixpoint, so the canonical form is the fixpoint of parse-and-print), and
 * compares that against what is on disk. In its default "check" mode, it
 * prints a colored unified {@link Diff} for every file that diverges from
 * the canonical form and fails the build. When {@link #autofix} is turned
 * on (via the {@code eo.autoFix} property), it overwrites the divergent
 * files with their canonical form instead of failing, much like
 * {@code gofmt -w} or {@code spotless:apply}.</p>
 *
 * <p>Parsing a source is by far the costliest thing this goal does, so it
 * does it as few times as it can: the sources are checked concurrently,
 * through {@link Threaded}, the way every other goal walks them, and each
 * settling pass keeps the tree it parsed instead of parsing the same text
 * again to lay it out.</p>
 *
 * @since 0.57.0
 * @todo #6627:30min Verify the tree read back from {@link Parsing#DIR}.
 *  The one this goal walks itself goes through {@link #parsed(Path, String)},
 *  which rejects a truncated or placeholder-carrying tree; the one read back
 *  is printed over its source unchecked.
 * @todo #6627:30min Name the local package objects in one place.
 *  This goal and {@link Parsing} each build the same list their own way, and
 *  the un-homing is only exact while the two agree, so they must not drift.
 * @todo #6627:30min Read the parsed tree through an object of its own.
 *  It is read and un-homed inside {@link #walked(TjForeign, String, String)},
 *  where neither half can be tested apart from the mojo.
 */
@Mojo(
    name = "format",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjFormat extends MjPenalties {

    /**
     * The most parse-and-print passes taken to settle the moniker layout
     * before giving up.
     *
     * <p>Merging a moniker onto its first bare reference (#5739) is not a
     * single-pass fixpoint: the canonical attribute ordering of #5706 can
     * move which reference is "first", so one print may shift a moniker and
     * a further print settle it. The structural form is therefore reached by
     * iterating parse-and-print until it stops changing; this cap keeps a
     * pathological non-converging source from looping forever.</p>
     */
    private static final int SETTLE = 8;

    /**
     * Overwrite divergent sources with their canonical form instead of
     * failing the build.
     */
    @Parameter(
        alias = "autoFix",
        property = "eo.autoFix",
        required = true,
        defaultValue = "false"
    )
    private boolean autofix;

    /**
     * Ctor.
     */
    public MjFormat() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final long start = System.currentTimeMillis();
        try (TjsForeign tojos = this.tojos()) {
            final Collection<TjForeign> sources = tojos.withSources();
            final String objects = sources.stream()
                .map(TjForeign::identifier)
                .filter(id -> id.contains("."))
                .distinct()
                .sorted()
                .collect(Collectors.joining(" "));
            this.report(
                sources.size(),
                new Threaded<>(sources, tojo -> this.reformat(tojo, objects)).total(),
                System.currentTimeMillis() - start
            );
        }
    }

    private int reformat(final TjForeign tojo, final String objects) throws IOException {
        final Path source = tojo.source();
        final String actual = new UncheckedText(new TextOf(source)).asString();
        final String canonical = this.canonical(tojo, actual, objects);
        final Diff diff = new Diff(actual, canonical);
        final int diverged;
        if (diff.same()) {
            diverged = 0;
        } else {
            diverged = 1;
            if (this.autofix) {
                new Saved(canonical, source).value();
                Logger.info(this, "Reformatted %[file]s", source);
            } else {
                Logger.warn(
                    this,
                    "%[file]s is not formatted canonically:%n%s",
                    source,
                    diff.colored()
                );
            }
        }
        return diverged;
    }

    private String canonical(
        final TjForeign tojo, final String source, final String objects
    ) throws IOException {
        final Path path = tojo.source();
        String structure = source;
        XML tree = MjFormat.walked(tojo, structure, objects);
        Optional<String> settled = Optional.empty();
        for (int pass = 0; pass < MjFormat.SETTLE; ++pass) {
            final String printed = new Xmir(tree).toEO();
            if (printed.equals(structure)) {
                settled = Optional.of(printed);
                break;
            }
            structure = printed;
            tree = MjFormat.parsed(path, structure);
        }
        final String canon;
        if (settled.isPresent() && this.weights().isEmpty()) {
            canon = settled.get();
        } else {
            canon = new Xmir(tree, this.weights()).toEO();
        }
        return canon;
    }

    private static XML walked(
        final TjForeign tojo, final String structure, final String objects
    ) throws IOException {
        final XML tree;
        if (tojo.notParsed()) {
            tree = MjFormat.parsed(tojo.source(), structure);
        } else {
            final XML saved = new XMLDocument(tojo.xmir());
            for (final XML blank : saved.nodes("//text()[not(normalize-space())][../*]")) {
                blank.inner().getParentNode().removeChild(blank.inner());
            }
            tree = new Xsline(
                new StClasspath(
                    "/org/eolang/maven/format/unhome-package.xsl",
                    String.format("objects %s", objects)
                )
            ).pass(saved);
        }
        return tree;
    }

    private static XML parsed(final Path source, final String structure) throws IOException {
        final XML xmir = new EoSyntax(structure).parsed();
        final long errors = new Xnav(xmir.inner())
            .element("object")
            .element("errors")
            .elements(Filter.withName("error"))
            .filter(MjFormat::severe)
            .count();
        if (errors > 0L
            && (MjFormat.truncated(xmir, structure)
                || MjFormat.placeholder(xmir)
                || MjFormat.lossy(xmir))) {
            throw new IllegalStateException(
                String.format(
                    "%s does not fully parse (%d error(s) found) and part of it was lost, won't format it",
                    source, errors
                )
            );
        }
        return xmir;
    }

    private static boolean lossy(final XML xmir) {
        return new Xnav(xmir.inner())
            .path("//errors/error[@lossy]")
            .findAny()
            .isPresent();
    }

    private static boolean truncated(final XML xmir, final String structure) {
        final String[] lines = structure.split(String.valueOf('\n'), -1);
        int last = 0;
        for (int idx = 0; idx < lines.length; ++idx) {
            if (!lines[idx].isBlank()) {
                last = idx + 1;
            }
        }
        return new Xnav(xmir.inner())
            .path("//o[@line]")
            .mapToInt(node -> Integer.parseInt(node.attribute("line").text().orElse("0")))
            .max()
            .orElse(0) < last;
    }

    private static boolean placeholder(final XML xmir) {
        return new Xnav(xmir.inner())
            .path("//o")
            .anyMatch(MjFormat::empty);
    }

    private static boolean empty(final Xnav obj) {
        return !obj.attribute("base").text().isPresent()
            && !obj.attribute("name").text().isPresent()
            && obj.elements(Filter.all()).count() == 0L
            && obj.text().orElse("").isBlank();
    }

    private static boolean severe(final Xnav error) {
        final String severity = error.attribute("severity").text().orElse("");
        return "error".equals(severity) || "critical".equals(severity);
    }

    private void report(final int total, final int divergent, final long millis) {
        if (divergent == 0) {
            Logger.info(
                this,
                "All %d EO source(s) are formatted canonically, took %[ms]s to check",
                total, millis
            );
        } else if (this.autofix) {
            Logger.info(
                this,
                "Reformatted %d of %d EO source(s), took %[ms]s",
                divergent, total, millis
            );
        } else {
            throw new IllegalStateException(
                String.format(
                    "%d of %d EO source(s) are not formatted canonically; %s",
                    divergent,
                    total,
                    "run with -Deo.autoFix to reformat them automatically"
                )
            );
        }
    }
}
