/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer.recursion;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import java.io.IOException;
import org.eolang.parser.EoSyntax;
import org.eolang.printer.Xmir;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Recursive handles survive receiver naming and printing.
 *
 * @since 0.61
 */
final class XmirTest {

    @ParameterizedTest
    @CsvSource({"^,^", "^,scan", "loop,^", "loop,scan"})
    void preservesRecursiveBindings(final String loop, final String scan) throws IOException {
        XML tree = new EoSyntax(
            String.join(
                System.lineSeparator(), "# Two recursive helpers.", "", "[] > main",
                String.format("  [^%s index] >> looped", loop.replace("^", "")),
                String.format("    %s.looped index > @", loop),
                String.format("  [^%s step] >> topbit", scan.replace("^", "")),
                String.format("    %s.topbit step > @", scan),
                "  looped 1 > result", "  topbit 2 > @", ""
            )
        ).parsed();
        for (int pass = 0; pass < 2; ++pass) {
            MatcherAssert.assertThat(
                "Both recursive bodies and their receiver bindings must survive printing",
                tree, Matchers.allOf(
                    Matchers.not(XhtmlMatchers.hasXPath("//errors/error")),
                    XhtmlMatchers.hasXPath(
                        "//o[@local='looped']/o[@name='φ' and @base=concat('ξ.ρ.', ../@name)]"
                    ),
                    XhtmlMatchers.hasXPath(
                        "//o[@local='topbit']/o[@name='φ' and @base=concat('ξ.ρ.', ../@name)]"
                    ),
                    XhtmlMatchers.hasXPath(
                        "//o[@name='result' and @base=concat('ξ.', ../o[@local='looped']/@name)]"
                    ),
                    XhtmlMatchers.hasXPath(
                        "//o[@name='main']/o[@base=concat('ξ.', ../o[@local='topbit']/@name)]"
                    )
                )
            );
            tree = new EoSyntax(new Xmir(tree).toEO()).parsed();
        }
    }

    @ParameterizedTest
    @CsvSource({
        "[],loop.recur,ξ.ρ.ρ.%s", "[],^.loop.recur,ξ.ρ.ρ.%s",
        "[loop],loop.recur,ξ.loop.recur"
    })
    void respectsReceiverScope(final String inner, final String call, final String base)
        throws IOException {
        final XML tree = new EoSyntax(
            String.join(
                System.lineSeparator(),
                "# Receiver capture respects shadowing and explicit hops.", "",
                "[] > main", "  [^loop] >> recur", String.format("    %s > inner", inner),
                String.format("      %s > @", call), "    inner > @", "  recur > @", ""
            )
        ).parsed();
        MatcherAssert.assertThat(
            "Receiver dispatch must bind to the selected lexical declaration",
            tree, XhtmlMatchers.hasXPath(
                String.format(
                    "//o[@name='inner']/o[@name='φ' and @base='%s']",
                    String.format(base, tree.xpath("//o[@local='recur']/@name").get(0))
                )
            )
        );
    }
}
