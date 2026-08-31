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
 * and {@code @pos} and its {@code @loc} has no {@code +} and no {@code .-}
 * in it. Those
 * attributes are already present by the time every shift except the last
 * one has run, so running that same prefix here and reading the result
 * gets the exact same set {@code to-java.xsl} would act on, structurally,
 * rather than by pattern-matching the Java it emits. Four shapes never get
 * a hit and are left out here: {@code classes.xsl} marks a whole atomic
 * class {@code @skip-java}, and {@code to-java.xsl} then writes no
 * {@code <java>} for it at all; the lambda marker of any atom (the
 * {@code o[@name='λ']} carrying {@code @atom}, whether the atom is a whole
 * class or a single attribute) is never the argument {@code to-java.xsl}
 * runs through {@code located} mode, only its parent is; a free attribute
 * {@code attrs.xsl} wraps into a {@code <void>}, since {@code to-java.xsl}
 * compiles it to a bare {@code AtVoid} and never runs it through
 * {@code located} mode either — the formal attributes a file's root
 * object declares are free this same way, which is why its own
 * declaration line never gets a hit (#6995); and, the same way, a void
 * attribute of any other formation (bracket-declared or written as
 * {@code ? >> name}) parses to an {@code o} whose own {@code @base} is
 * a void, the empty-set glyph — no body to dataize, so no {@code located} mode call and no
 * hit ever lands on its declaration line either (#7377).</p>
 *
 * <p>An anonymous formation is left out too. {@code anonymous-to-nested.xsl}
 * turns one into a nested class, and {@code to-java.xsl} never runs a
 * nested class through {@code located} mode, so no hit can ever land on
 * the line the {@code []} sits on. Its body still gets its own locations
 * — only the formation's own declaration line is left out (#7941).</p>
 *
 * <p>An atom attribute is left out along with everything it declares,
 * even though {@code to-java.xsl} does wrap it and its voids and the
 * runtime does record hits for them (#7055). Such an attribute has no
 * body in {@code .eo} at all — its body is the Java class the
 * {@code @atom} names — so the lines it occupies say nothing about how
 * much of the {@code .eo} source the tests exercised. Counting them
 * reads as coverage of code that is not there: the declaration line of
 * {@code [] > read /Q.bytes} shows up uncovered while the {@code ? >
 * offset} lines under it show up covered, both of them merely
 * declarations. The hits the runtime still appends for those locations
 * find no place in the manifest and are dropped, the same way a hit of
 * any location the manifest does not name is.</p>
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
     * hit under. An element with no {@code @loc} is not a location at
     * all: a parser error element carries a line and a position and
     * nothing else, and every other element of a parsed XMIR that
     * carries both carries a locator too.
     * @param xmir The XMIR a source was parsed and optimized into
     * @return The locations, in document order
     */
    Collection<String> locations(final XML xmir) {
        final XML passed = new Xsline(this.train).pass(xmir);
        final Collection<String> found = new LinkedHashSet<>();
        for (final XML located : passed.nodes(
            "//*[@line and @pos and @loc and not(contains(@loc,'+')) and not(contains(@loc,'.-')) and not(@atom) and not(@skip-java) and not(self::class) and not(@base=codepoints-to-string(8709)) and not(ancestor::void) and not(ancestor-or-self::*[o[@atom]]) and (@base or @name)]"
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
