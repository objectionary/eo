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
import java.util.EnumMap;
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
     */
    @Parameter(property = "eo.width")
    private Integer width;

    /**
     * Ctor.
     */
    public MjFormat() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final long start = System.currentTimeMillis();
        final Collection<TjForeign> sources = this.scopedTojos().withSources();
        this.report(
            sources.size(),
            new Threaded<>(sources, tojo -> this.reformat(tojo.source())).total(),
            System.currentTimeMillis() - start
        );
    }

    /**
     * Reformat a single source, either fixing it or reporting the diff.
     * @param source The path of the {@code .eo} file
     * @return One if the file diverged from the canonical form, zero otherwise
     * @throws IOException If fails to read or write the file
     */
    private int reformat(final Path source) throws IOException {
        final String actual = new UncheckedText(new TextOf(source)).asString();
        final String canonical = this.canonical(source, actual);
        final Diff diff = new Diff(actual, canonical);
        final int diverged;
        if (diff.same()) {
            diverged = 0;
        } else {
            diverged = 1;
            if (this.autoFix) {
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

    /**
     * The canonical form of a source.
     *
     * <p>A binding with several bare references merges onto whichever is
     * "first" in the canonically ordered body (#5706, #5739), and that
     * choice can move on the next pass, so a single parse-and-print is not
     * always a fixpoint. The moniker's home is decided by the structure
     * alone (attribute order and the XSL merge), not by the layout weights,
     * so this settles it at the default layout and then lays the settled
     * structure out once with the configured weights. The settling is
     * bounded by {@link #SETTLE}; a
     * source that has not settled by then is laid out as-is and reported
     * divergent, so it fails loudly instead of looping.</p>
     *
     * @param path The path of the {@code .eo} file, for the error message
     * @param source The current text of the {@code .eo} file
     * @return The canonical text
     * @throws IOException If fails to parse the source
     */
    private String canonical(final Path path, final String source) throws IOException {
        String structure = source;
        XML tree = MjFormat.parsed(path, structure);
        for (int pass = 0; pass < MjFormat.SETTLE; ++pass) {
            final String next = new Xmir(tree).toEO();
            if (next.equals(structure)) {
                break;
            }
            structure = next;
            tree = MjFormat.parsed(path, structure);
        }
        return new Xmir(tree, this.weights()).toEO();
    }

    /**
     * Parse a source and verify none of the source was lost in the process.
     * A source with a genuine syntax error still yields an XMIR carrying
     * {@code <errors>} (see {@link Parsing}), but the parser also recovers
     * from many purely cosmetic layout violations (e.g. extra blank lines,
     * exactly what this goal exists to fix) the same way, tagged with the
     * same {@code severity="error"} and no other distinguishing marker.
     * Rejecting every {@code <errors>} entry, as suggested by the issue this
     * fixes, would therefore also reject this goal's own core use case.
     *
     * <p>What actually matters is whether the recovered tree still covers
     * the whole source: a genuine syntax error (e.g. an unterminated paren
     * group) makes the parser stop incorporating source lines into the tree
     * from that point on, while a cosmetic layout error does not lose any
     * line. So instead of trusting {@code severity} alone, this also checks
     * that some {@code <o>} element references a line at or past the
     * source's last non-blank line; if none does, part of the source was
     * never parsed at all, and this goal would otherwise print back (or, in
     * {@code -Deo.autoFix} mode, save) a truncated version of it.</p>
     *
     * <p>Line coverage is necessary but not sufficient: the parser recovers
     * from some <em>structural</em> errors not by dropping the remaining
     * lines but by substituting an empty placeholder {@code <o>} for the
     * piece it could not build and then carrying on. Coverage stays intact,
     * yet the tree no longer describes the source, so this also rejects a
     * tree carrying such a {@link #placeholder(XML) placeholder}.</p>
     *
     * <p>Neither heuristic catches every lossy recovery: the parser can
     * also drop a single binding line entirely and carry straight on to
     * its siblings, which neither truncates line coverage (later siblings
     * still reach the source's last line) nor leaves a placeholder (there
     * is simply nothing where the binding used to be) — see #6649. That
     * one recovery site tags its own {@code <error>} with {@code lossy}
     * (see {@link Emit#error(int, int, String, boolean)}), so this also
     * rejects a tree carrying such an error, without having to reject
     * every {@code <errors>} entry the way the issue that reported this
     * gap first suggested, which would also reject this goal's own core
     * use case of recovering from purely cosmetic layout violations.</p>
     *
     * @param source The path of the {@code .eo} file, for the error message
     * @param structure The current text of the {@code .eo} file
     * @return The parsed XMIR
     * @throws IOException If fails to parse the source
     */
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

    /**
     * Whether any error carries the {@code lossy} marker, meaning its own
     * recovery dropped the whole construct it was building.
     * @param xmir The parsed XMIR
     * @return TRUE if such an error is present
     */
    private static boolean lossy(final XML xmir) {
        return new Xnav(xmir.inner())
            .path("//errors/error[@lossy]")
            .findAny()
            .isPresent();
    }

    /**
     * Whether the parsed tree stops short of the source's last non-blank
     * line, meaning some of the source was never incorporated into it.
     * @param xmir The parsed XMIR
     * @param structure The source text that was parsed
     * @return TRUE if part of the source was not recovered
     */
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

    /**
     * Whether the parsed tree carries a recovered placeholder node.
     *
     * <p>Recovering from a structural error, the parser can substitute an
     * empty {@code <o>} — one with no {@code base}, no {@code name}, no
     * child elements and no text — for a node it could not build. A
     * reversed dispatch left without a receiver, for instance, becomes such
     * a node, which the printer can only render as {@code []}. Formatting a
     * tree that carries one would silently rewrite the source into a
     * different one the parser happens to accept.</p>
     *
     * @param xmir The parsed XMIR
     * @return TRUE if a recovered placeholder stands in for lost source
     */
    private static boolean placeholder(final XML xmir) {
        return new Xnav(xmir.inner())
            .path("//o")
            .anyMatch(MjFormat::empty);
    }

    /**
     * Whether an {@code <o>} is an empty recovery placeholder, i.e. it
     * carries neither a {@code base} nor a {@code name} attribute, has no
     * child elements and holds no text.
     * @param obj The {@code <o>} element
     * @return TRUE if the element is such a placeholder
     */
    private static boolean empty(final Xnav obj) {
        return !obj.attribute("base").text().isPresent()
            && !obj.attribute("name").text().isPresent()
            && obj.elements(Filter.all()).count() == 0L
            && obj.text().orElse("").isBlank();
    }

    /**
     * Whether an {@code <error>} element carries error or critical severity.
     * @param error The error element
     * @return TRUE if the error is severe
     */
    private static boolean severe(final Xnav error) {
        final String severity = error.attribute("severity").text().orElse("");
        return "error".equals(severity) || "critical".equals(severity);
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
        return map;
    }

    /**
     * Report the outcome, failing the build if needed.
     * @param total The number of registered sources
     * @param divergent How many sources diverged from the canonical form
     * @param millis The elapsed time, in milliseconds
     */
    private void report(final int total, final int divergent, final long millis) {
        if (divergent == 0) {
            Logger.info(
                this,
                "All %d EO source(s) are formatted canonically, took %[ms]s to check",
                total, millis
            );
        } else if (this.autoFix) {
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
