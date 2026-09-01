/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Optional;
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
 * @todo #6263:30min Parse every {@code .eo} source once per build.
 *  This goal parses each source and throws the tree away, and then the
 *  {@code compile} goal parses the very same text again seconds later,
 *  so a clean build of {@code eo-runtime} parses its 170 sources twice.
 *  Hand the settled tree of {@link #canonical(Path, String)} over to
 *  {@link Parsing} instead, keyed by the source hash the way
 *  {@link GlobalCache} already keys its footprints, so that the second
 *  parse is skipped when the format goal has just produced the same tree.
 */
@Mojo(
    name = "format",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjFormat extends MjPenalties {

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
            this.report(
                sources.size(),
                new Threaded<>(sources, tojo -> this.reformat(tojo.source())).total(),
                System.currentTimeMillis() - start
            );
        }
    }

    private int reformat(final Path source) throws IOException {
        final String actual = new UncheckedText(new TextOf(source)).asString();
        final String canonical = this.canonical(source, actual);
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

    private String canonical(final Path path, final String source) throws IOException {
        String structure = source;
        XML tree = MjFormat.parsed(path, structure);
        Optional<String> settled = Optional.empty();
        final int settle = 8;
        for (int pass = 0; pass < settle; ++pass) {
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
