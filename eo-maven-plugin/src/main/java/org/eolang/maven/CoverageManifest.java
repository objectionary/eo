/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;

/**
 * The locations a transpiled XMIR would have {@code PhCoverage} wrapped
 * around, derived the same way {@code to-java.xsl} itself decides it,
 * but without deriving it from generated Java text.
 *
 * <p>{@code to-java.xsl} (the last shift of {@link Transpilation#XSLS})
 * wraps a location into {@code PhCoverage} when it carries {@code @line}
 * and {@code @pos} and its {@code @loc} has no {@code +} in it. Those
 * attributes are already present by the time every shift except the last
 * one has run, so running that same prefix here and reading the result
 * gets the exact same set {@code to-java.xsl} would act on, structurally,
 * rather than by pattern-matching the Java it emits.</p>
 *
 * @since 0.75.0
 * @todo #5757:30min Wire this into a Maven goal that reads the raw
 *  {@code loc:line:pos} hits file {@code PhCoverage} appends to at
 *  runtime, matches it against {@link #locations(XML)} for every
 *  transpiled source, and renders an LCOV tracefile. Split out of this
 *  PR solely to respect the 200-line pull request size cap; the wiring
 *  is a separate, immediately-following PR against the same issue.
 */
final class CoverageManifest {

    /**
     * Every shift of the transpile train except {@code to-java.xsl}.
     */
    private final Train<Shift> train = new TrClasspath<Shift>(
        Arrays.copyOf(Transpilation.XSLS, Transpilation.XSLS.length - 1)
    ).back();

    /**
     * The locations a transpile of this XMIR would instrument, each as
     * {@code loc:line:pos}, the same string {@code PhCoverage} records a
     * hit under.
     * @param xmir The XMIR a source was parsed and optimized into
     * @return The locations, in document order
     */
    Collection<String> locations(final XML xmir) {
        final XML passed = new Xsline(this.train).pass(xmir);
        final Collection<String> found = new LinkedHashSet<>();
        for (final XML located : passed.nodes(
            "//*[@line and @pos and not(contains(@loc,'+'))]"
        )) {
            found.add(
                String.format(
                    "%s:%s:%s",
                    located.xpath("./@loc").get(0),
                    located.xpath("./@line").get(0),
                    located.xpath("./@pos").get(0)
                )
            );
        }
        return found;
    }
}
