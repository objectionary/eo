/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Seen}.
 * @since 0.70.0
 */
final class SeenTest {

    @Test
    void readsBackEveryTypeACallerWasSeenPassing() {
        MatcherAssert.assertThat(
            "the two types the table witnesses must both come back, but they didnt",
            new Xembler(
                SeenTest.drawn(
                    "<provides><type id='Φ.bool.and'><attr name='x' type='Φ.bool.and.x'",
                    " void='true'><witnessed><union><ref loc='Φ.true'/>",
                    "<ref loc='Φ.false'/></union></witnessed></attr></type></provides>"
                )
            ).xmlQuietly(),
            XhtmlMatchers.hasXPaths(
                "/seen/ref[@loc='Φ.true']",
                "/seen/ref[@loc='Φ.false']"
            )
        );
    }

    @Test
    void saysThatADatumWentIn() {
        MatcherAssert.assertThat(
            "a void filled with bytes must own up to it, but it didnt",
            new Xembler(
                SeenTest.drawn(
                    "<provides><type id='Φ.bool.and'><attr name='x' type='Φ.bool.and.x'",
                    " void='true'><witnessed><data/></witnessed></attr></type></provides>"
                )
            ).xmlQuietly(),
            XhtmlMatchers.hasXPath("/seen/data")
        );
    }

    @Test
    void readsBackTheCallerThatPassesOnAVoid() {
        MatcherAssert.assertThat(
            "the void a caller passes on must come back with its locator, but it didnt",
            new Xembler(
                SeenTest.drawn(
                    "<provides><type id='Φ.bool.and'><attr name='x' type='Φ.bool.and.x'",
                    " void='true'><witnessed><union><ref loc='Φ.true'/>",
                    "<var id='Φ.app.y'/></union></witnessed></attr></type></provides>"
                )
            ).xmlQuietly(),
            XhtmlMatchers.hasXPath("/seen/var[@id='Φ.app.y']")
        );
    }

    @Test
    void keepsAVoidNobodyFillsInTheAnswer() {
        MatcherAssert.assertThat(
            "a void with no witnesses must still be listed, but it was left out",
            new Seen(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.bool.and'><attr name='x'",
                        " type='Φ.bool.and.x' void='true'/></type></provides>"
                    )
                )
            ).all(),
            Matchers.hasKey("Φ.bool.and.x")
        );
    }

    private static Directives drawn(final String... lines) {
        final Directives dirs = new Directives().add("seen");
        for (final Type witness
            : new Seen(new XMLDocument(String.join("", lines))).all().get("Φ.bool.and.x")) {
            dirs.append(witness.directives());
        }
        return dirs.up();
    }
}
