/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link Same}.
 *
 * <p>What the checker understands about a program is checked by the packs of
 * the Maven plugin, which read EO source. This is the collapsing of copies
 * underneath, which knows nothing about EO and everything about a table of
 * links, so it is asked here directly.</p>
 *
 * @since 0.68.0
 */
final class SameTest {

    @Test
    void callsCopyByTheNameOfWhatItCopies() {
        MatcherAssert.assertThat(
            "a copy must go by the name of the object it copies, but it didnt",
            new Same(
                new XMLDocument(
                    "<links><type id='Φ.app.φ' copy='Φ.jar'/></links>"
                )
            ).names(),
            Matchers.hasEntry("Φ.app.φ", "Φ.jar")
        );
    }

    @Test
    void followsChainOfCopiesToItsEnd() {
        MatcherAssert.assertThat(
            "a copy of a copy must go by the name at the end of the chain, but it didnt",
            new Same(
                new XMLDocument(
                    String.join(
                        "",
                        "<links>",
                        "<type id='Φ.app.φ.α0' copy='Φ.app.lid'/>",
                        "<type id='Φ.app.lid' copy='Φ.jar'/>",
                        "</links>"
                    )
                )
            ).names(),
            Matchers.hasEntry("Φ.app.φ.α0", "Φ.jar")
        );
    }

    @Test
    void saysNothingAboutTypeThatCopiesNothing() {
        MatcherAssert.assertThat(
            "a type no link mentions must be left out, since it goes by its own name",
            new Same(
                new XMLDocument("<links><type id='Φ.app.φ' copy='Φ.jar'/></links>")
            ).names(),
            Matchers.not(Matchers.hasKey("Φ.jar"))
        );
    }

    @Test
    @Timeout(10L)
    void walksOutOfCopiesThatCopyEachOther() {
        MatcherAssert.assertThat(
            "copies pointing at each other must not be walked forever, but they were",
            new Same(
                new XMLDocument(
                    String.join(
                        "",
                        "<links>",
                        "<type id='Φ.honey' copy='Φ.comb'/>",
                        "<type id='Φ.comb' copy='Φ.honey'/>",
                        "</links>"
                    )
                )
            ).names(),
            Matchers.hasKey("Φ.honey")
        );
    }
}
