/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.EnumMap;
import java.util.LinkedList;
import java.util.Map;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.EoSyntax;
import org.eolang.printer.PenaltyKey;
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
 * the canonical form and fails the build. When {@link #autoFix} is turned
 * on (via the {@code eo.autoFix} property), it overwrites the divergent
 * files with their canonical form instead of failing, much like
 * {@code gofmt -w} or {@code spotless:apply}.</p>
 *
 * @since 0.57.0
 */
@Mojo(
    name = "format",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjFormat extends MjSafe {

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
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.autoFix", required = true, defaultValue = "false")
    private boolean autoFix;

    /**
     * Points charged for each level of indentation on a line.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.penaltyIndent")
    private Integer penaltyIndent;

    /**
     * Points charged for each opening parenthesis.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.penaltyBracket")
    private Integer penaltyBracket;

    /**
     * Points charged for each character past the allowed width.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.penaltyExcess")
    private Integer penaltyExcess;

    /**
     * The column after which characters start being charged.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.width")
    private Integer width;

    /**
     * The width of a single indentation level, in spaces.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(property = "eo.step")
    private Integer step;

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void exec() throws IOException {
        final long start = System.currentTimeMillis();
        final Collection<TjForeign> sources = this.scopedTojos().withSources();
        final Collection<Path> divergent = new LinkedList<>();
        for (final TjForeign tojo : sources) {
            if (this.reformat(tojo.source())) {
                divergent.add(tojo.source());
            }
        }
        this.report(sources.size(), divergent, System.currentTimeMillis() - start);
    }

    /**
     * Reformat a single source, either fixing it or reporting the diff.
     * @param source The path of the {@code .eo} file
     * @return TRUE if the file diverged from the canonical form
     * @throws IOException If fails to read or write the file
     */
    private boolean reformat(final Path source) throws IOException {
        final String actual = new UncheckedText(new TextOf(source)).asString();
        final String canonical = this.canonical(actual);
        final Diff diff = new Diff(actual, canonical);
        final boolean diverged = !diff.same();
        if (diverged && this.autoFix) {
            new Saved(canonical, source).value();
            Logger.info(this, "Reformatted %[file]s", source);
        } else if (diverged) {
            Logger.warn(
                this,
                "%[file]s is not formatted canonically:%n%s",
                source,
                diff.colored()
            );
        }
        return diverged;
    }

    /**
     * The canonical form of a source.
     *
     * <p>A binding with several bare references merges onto whichever is
     * "first" in the canonically ordered body (#5706, #5739), and that
     * choice can move on the next pass, so a single parse-and-print is not
     * always a fixpoint. The moniker's home is decided by the structure
     * alone (attribute order and the XSL merge), not by the layout weights,
     * so this settles it at the default, always-reparseable layout — a
     * custom {@link #step} would emit indentation the parser cannot read
     * back — and then lays the settled structure out once with the
     * configured weights. The settling is bounded by {@link #SETTLE}; a
     * source that has not settled by then is laid out as-is and reported
     * divergent, so it fails loudly instead of looping.</p>
     *
     * @param source The current text of the {@code .eo} file
     * @return The canonical text
     * @throws IOException If fails to parse the source
     */
    private String canonical(final String source) throws IOException {
        String structure = source;
        for (int pass = 0; pass < MjFormat.SETTLE; ++pass) {
            final String next = new Xmir(new EoSyntax(structure).parsed()).toEO();
            if (next.equals(structure)) {
                break;
            }
            structure = next;
        }
        return new Xmir(new EoSyntax(structure).parsed(), this.weights()).toEO();
    }

    /**
     * Assemble the overridden penalty weights from the Maven properties.
     *
     * <p>Only the properties that the user actually set are put into the
     * map; every absent key falls back to its {@link PenaltyKey#fallback()}
     * default inside the printer.</p>
     *
     * @return The weights, keyed by {@link PenaltyKey}
     */
    private Map<PenaltyKey, Integer> weights() {
        final Map<PenaltyKey, Integer> map = new EnumMap<>(PenaltyKey.class);
        if (this.penaltyIndent != null) {
            map.put(PenaltyKey.INDENT, this.penaltyIndent);
        }
        if (this.penaltyBracket != null) {
            map.put(PenaltyKey.BRACKET, this.penaltyBracket);
        }
        if (this.penaltyExcess != null) {
            map.put(PenaltyKey.EXCESS, this.penaltyExcess);
        }
        if (this.width != null) {
            map.put(PenaltyKey.WIDTH, this.width);
        }
        if (this.step != null) {
            map.put(PenaltyKey.STEP, this.step);
        }
        return map;
    }

    /**
     * Report the outcome, failing the build if needed.
     * @param total The number of registered sources
     * @param divergent The sources that diverged from the canonical form
     * @param millis The elapsed time, in milliseconds
     */
    private void report(final int total, final Collection<Path> divergent, final long millis) {
        if (divergent.isEmpty()) {
            Logger.info(
                this, "All %d EO source(s) are formatted canonically in %[ms]s", total, millis
            );
        } else if (this.autoFix) {
            Logger.info(
                this,
                "Reformatted %d of %d EO source(s) in %[ms]s",
                divergent.size(), total, millis
            );
        } else {
            throw new IllegalStateException(
                String.format(
                    "%d of %d EO source(s) are not formatted canonically; %s",
                    divergent.size(),
                    total,
                    "run with -Deo.autoFix to reformat them automatically"
                )
            );
        }
    }
}
