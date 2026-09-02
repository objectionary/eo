/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Noted}.
 * @since 0.71.0
 */
final class NotedTest {

    @Test
    void readsWhatANodeSays() {
        MatcherAssert.assertThat(
            "the locator written on the node must come back, but it didnt",
            new Noted(
                new Xnav(
                    new XMLDocument("<o loc='Φ.ω.k' name='next'/>").inner()
                ).element("o")
            ).says("loc"),
            Matchers.equalTo("Φ.ω.k")
        );
    }

    @Test
    void saysNothingAboutAnAttributeNobodyWrote() {
        MatcherAssert.assertThat(
            "an attribute that is not there must come back empty, but it didnt",
            new Noted(
                new Xnav(
                    new XMLDocument("<o loc='Φ.j'/>").inner()
                ).element("o")
            ).says("base"),
            Matchers.emptyString()
        );
    }

    @Test
    void readsWhatANodeOfADocumentSays() {
        MatcherAssert.assertThat(
            "the name written on a node found in a document must come back, but it didnt",
            new Noted(
                new XMLDocument("<object><o name='ζ'/></object>").nodes("/object/o").get(0)
            ).says("name"),
            Matchers.equalTo("ζ")
        );
    }
}
