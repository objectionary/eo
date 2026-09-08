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
 * Test case for {@link Site}.
 * @since 0.71.0
 */
final class SiteTest {

    @Test
    void tellsWhereItIsWritten() {
        MatcherAssert.assertThat(
            "the locator of the dispatch must come back, but it didnt",
            new Site(
                new Xnav(
                    new XMLDocument("<o base='.plus' loc='Φ.ω.k'/>").inner()
                ).element("o")
            ).made(),
            Matchers.equalTo("Φ.ω.k")
        );
    }

    @Test
    void tellsWhichNameItTakes() {
        MatcherAssert.assertThat(
            "the name must come back without the dot that says it is a dispatch, but it didnt",
            new Site(
                new Xnav(
                    new XMLDocument("<o base='.eq' loc='Φ.j'/>").inner()
                ).element("o")
            ).name(),
            Matchers.equalTo("eq")
        );
    }

    @Test
    void takesItsNameFromTheChildThatIsNoArgument() {
        MatcherAssert.assertThat(
            "the object the name is taken from must be the child with no place, but it wasnt",
            new Site(
                new Xnav(
                    new XMLDocument(
                        String.join(
                            "",
                            "<o base='.pow' loc='Φ.q'>",
                            "<o as='α0' loc='Φ.q.α0'/>",
                            "<o loc='Φ.q.ρ'/>",
                            "</o>"
                        )
                    ).inner()
                ).element("o")
            ).bearer(),
            Matchers.equalTo("Φ.q.ρ")
        );
    }
}
