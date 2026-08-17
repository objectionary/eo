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
 * rather than by pattern-matching the Java it emits. An atom never gets a
 * hit: {@code classes.xsl} marks a whole atomic class {@code @skip-java}
 * and {@code to-java.xsl} then writes no {@code <java>} for it, while
 * {@code attrs.xsl} wraps an atomic attribute into an {@code atom} element
 * whose own line {@code to-java.xsl} never runs through {@code located}
 * mode. Both are left out here.</p>
 *
 * @since 0.75.0
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
            "//*[@line and @pos and not(contains(@loc,'+')) and not(self::atom) and not(@skip-java)]"
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
