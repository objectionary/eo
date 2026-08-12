/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Ungrouped}.
 *
 * <p>What the checker understands about a program is checked by the packs of
 * the Maven plugin, which read EO source. This is the reading of a table
 * underneath, which knows nothing about EO and everything about the document
 * {@link Grouped} writes, so it is asked here directly.</p>
 *
 * @since 0.68.0
 */
final class UngroupedTest {

    @Test
    void readsEveryCellOfRow() {
        MatcherAssert.assertThat(
            "a cell of a row must be read back whatever it is called, but one wasnt",
            new Ungrouped(
                new XMLDocument(
                    "<provides><type id='Φ.jar'><attr name='lid' mood='shut'/></type></provides>"
                ),
                Collections.emptyMap()
            ).rows().get("Φ.jar"),
            Matchers.hasItem(Matchers.hasEntry("mood", "shut"))
        );
    }

    @Test
    void keepsTypeAndItsAttributesTogether() {
        MatcherAssert.assertThat(
            "the row of a type and the rows of its attributes must end up together",
            new Ungrouped(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.jar' complete='true'>",
                        "<attr name='lid'/><attr name='spout'/>",
                        "</type></provides>"
                    )
                ),
                Collections.emptyMap()
            ).rows().get("Φ.jar"),
            Matchers.hasSize(3)
        );
    }

    @Test
    void putsRowsUnderNameOwnerGoesBy() {
        MatcherAssert.assertThat(
            "the rows of a copy must be found under the name of what it copies, but they werent",
            new Ungrouped(
                new XMLDocument("<needs><type id='Φ.app.φ'><attr name='lid'/></type></needs>"),
                Collections.singletonMap("Φ.app.φ", "Φ.jar")
            ).rows(),
            Matchers.hasKey("Φ.jar")
        );
    }
}
