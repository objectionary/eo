/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Provides}.
 * @since 0.67.0
 */
final class ProvidesTest {

    @Test
    void listsAttributesOfFormation() {
        MatcherAssert.assertThat(
            "the row of a formation must name every attribute it binds, but it doesnt",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.kettle' name='kettle'>",
                        "<o loc='Φ.kettle.steam' name='steam'/>",
                        "<o loc='Φ.kettle.whistle' name='whistle'/>",
                        "</o></object>"
                    )
                )
            ).asXml(),
            XhtmlMatchers.hasXPath(
                String.join(
                    "",
                    "/provides/type[@id='Φ.kettle'",
                    " and attr[@name='steam' and @type='Φ.kettle.steam']",
                    " and attr[@name='whistle' and @type='Φ.kettle.whistle']]"
                )
            )
        );
    }

    @Test
    void marksFormationComplete() {
        MatcherAssert.assertThat(
            "a formation we have seen entirely must be complete, but it isnt",
            new Provides(
                new XMLDocument(
                    "<object><o loc='Φ.lid' name='lid'/></object>"
                )
            ).asXml(),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.lid' and @complete='true']")
        );
    }

    @Test
    void marksVoidAttribute() {
        MatcherAssert.assertThat(
            "a void attribute must be marked as such, but it isnt",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.pipe' name='pipe'>",
                        "<o base='∅' loc='Φ.pipe.width' name='width'/>",
                        "</o></object>"
                    )
                )
            ).asXml(),
            XhtmlMatchers.hasXPath(
                "/provides/type[@id='Φ.pipe']/attr[@name='width' and @void='true']"
            )
        );
    }

    @Test
    void leavesAtomIncomplete() {
        MatcherAssert.assertThat(
            "an atom hides its body in Java, so its row cannot be complete, but it is",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.tick' name='tick'>",
                        "<o atom='Φ.number' loc='Φ.tick.λ' name='λ'/>",
                        "</o></object>"
                    )
                )
            ).asXml(),
            XhtmlMatchers.hasXPath(
                "/provides/type[@id='Φ.tick' and @complete='false' and not(attr)]"
            )
        );
    }

    @Test
    void skipsApplications() {
        MatcherAssert.assertThat(
            "an application provides nothing of its own, so it must have no row, but it has",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.lamp' name='lamp'>",
                        "<o base='ξ.bulb' loc='Φ.lamp.φ' name='φ'/>",
                        "</o></object>"
                    )
                )
            ).asXml(),
            XhtmlMatchers.hasXPath("/provides[count(type)=1 and type/@id='Φ.lamp']")
        );
    }

    @Test
    void skipsData() {
        MatcherAssert.assertThat(
            "bytes are not a formation, so they must have no row, but they have",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.two' name='two'>",
                        "<o base='Φ.bytes' loc='Φ.two.φ' name='φ'>",
                        "<o as='α0' loc='Φ.two.φ.α0'>02-</o>",
                        "</o></o></object>"
                    )
                )
            ).asXml(),
            XhtmlMatchers.hasXPath("/provides[count(type)=1 and type/@id='Φ.two']")
        );
    }
}
