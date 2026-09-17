/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Hollows}.
 *
 * @since 0.73.0
 */
final class HollowsTest {

    @Test
    void namesTheVoidsAndNothingElse() {
        MatcherAssert.assertThat(
            "the voids of the table must be the ones marked so, but they werent",
            new Hollows(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.oak'>",
                        "<attr name='leaf' type='Φ.oak.leaf' void='true'/>",
                        "<attr name='bark' type='Φ.oak.bark'/>",
                        "<attr name='root' type='Φ.oak.root' void='false'/>",
                        "</type></provides>"
                    )
                )
            ).all(),
            Matchers.contains("Φ.oak.leaf")
        );
    }

    @Test
    void keepsTheOrderTheTableNamesThemIn() {
        MatcherAssert.assertThat(
            "the voids must come back in the order the table wrote them, but they didnt",
            new Hollows(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides>",
                        "<type id='Φ.elm'>",
                        "<attr name='leaf' type='Φ.elm.leaf' void='true'/>",
                        "</type>",
                        "<type id='Φ.oak'>",
                        "<attr name='bud' type='Φ.oak.bud' void='true'/>",
                        "<attr name='leaf' type='Φ.oak.leaf' void='true'/>",
                        "</type>",
                        "</provides>"
                    )
                )
            ).all(),
            Matchers.contains("Φ.elm.leaf", "Φ.oak.bud", "Φ.oak.leaf")
        );
    }
}
