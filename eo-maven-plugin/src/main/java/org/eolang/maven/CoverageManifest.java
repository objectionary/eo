/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.Train;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.function.Function;

/**
 * The full set of locations the transpiler wrapped into {@code PhCoverage},
 * one line per location: symbolic locator and source line, tab-separated.
 * Built once per {@code transpile} run (see {@link Transpiling}), then read
 * back by {@link MjCoverageReport}, which merges it against the raw hits
 * recorded at runtime by {@code PhCoverage} itself, to tell which
 * instrumented lines were actually touched. The manifest does not record a
 * source file for each location: {@link MjCoverageReport} resolves that by
 * matching a locator's leading object id against the already-existing
 * {@code eo-foreign} catalog, instead of duplicating that mapping here.
 * @since 0.62.0
 */
final class CoverageManifest implements Coverage {

    /**
     * The condition {@code to-java.xsl} tests in its own
     * {@code mode="located"} template before emitting a wrapper.
     */
    private static final String WHEN = "@line and @pos and not(contains(@loc, '+'))";

    /**
     * The {@code @base} an unbound (void) attribute carries.
     */
    private static final String VOID = "∅";

    /**
     * Selects exactly the elements that end up wrapped into {@code
     * PhCoverage}, in the XMIR as it stands right before the final
     * {@code to-java.xsl} shift runs, so that the manifest is derived from
     * that structure directly, not by pattern-matching the Java text
     * {@code to-java.xsl} emits.
     * <p>
     *     {@code to-java.xsl} emits a wrapper from a single place, its
     *     {@code mode="located"} template, and that template is reached from
     *     exactly four others: {@code match="atom"} applies it to the atom's
     *     first {@code o} child, {@code match="abstract"} applies it to
     *     itself, and the two {@code mode="object"} templates, for a plain
     *     object ({@code @base} set and not starting with a dot) and for a
     *     method call ({@code @base} starting with a dot, with children),
     *     apply it to themselves. The four branches below mirror those four,
     *     each carrying {@link #WHEN}, and the third one also leaves out
     *     {@link #VOID}, the placeholder of an unbound attribute, which is a
     *     hole rather than an object: {@code to-java.xsl} turns it into an
     *     {@code AtVoid} through its own {@code match="void"} template and
     *     never dispatches on it as an object. Everything else that happens
     *     to carry the same attributes, a formation with no {@code @base}
     *     above all, is left out too, because no wrapper is ever emitted for
     *     it and counting it would inflate the {@code LF} of the report.
     * </p>
     */
    private static final String LOCATED = String.format(
        String.join(
            " | ",
            "//abstract[%1$s]",
            "//atom/o[1][%1$s]",
            String.format(
                "//o[@base and @base!='' and @base!='%s' and not(starts-with(@base, '.')) and %%1$s]",
                CoverageManifest.VOID
            ),
            "//o[starts-with(@base, '.') and * and %1$s]"
        ),
        CoverageManifest.WHEN
    );

    /**
     * The raw coverage hits file this manifest is paired with.
     */
    private final Path hits;

    /**
     * Ctor.
     * @param touched The raw coverage hits file this manifest is paired with
     */
    CoverageManifest(final Path touched) {
        this.hits = touched;
    }

    @Override
    public boolean tracked() {
        return true;
    }

    @Override
    public XML located(
        final XML xmir, final Function<XML, XML> shifts, final Collection<String> found
    ) {
        final XML located = shifts.apply(xmir);
        for (final XML element : located.nodes(CoverageManifest.LOCATED)) {
            found.add(
                String.format(
                    "%s\t%s",
                    element.xpath("@loc").get(0),
                    element.xpath("@line").get(0)
                )
            );
        }
        return located;
    }

    @Override
    public Train<Shift> pending(final Train<Shift> whole, final Train<Shift> last) {
        return last;
    }

    @Override
    public void save(final Collection<String> found) throws IOException {
        Files.write(
            this.path(),
            found,
            StandardCharsets.UTF_8,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING
        );
    }

    /**
     * Where this manifest is stored: a sibling of the hits file.
     * @return The path
     */
    Path path() {
        return Paths.get(String.format("%s.manifest", this.hits));
    }
}
