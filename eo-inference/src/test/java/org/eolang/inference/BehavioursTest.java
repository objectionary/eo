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
 * Test case for {@link Behaviours}.
 *
 * @since 0.73.0
 */
final class BehavioursTest {

    @Test
    void readsTheNameATypeBehavesAs() {
        MatcherAssert.assertThat(
            "the name on the row must come back against the type, but it didnt",
            new Behaviours(
                new XMLDocument("<provides><type id='Φ.alias' reduced='Φ.oak'/></provides>")
            ).all(),
            Matchers.hasEntry("Φ.alias", "Φ.oak")
        );
    }

    @Test
    void leavesOutATypeThatBehavesAsItself() {
        MatcherAssert.assertThat(
            "a type with no other name to go by must be left out, but it was let in",
            new Behaviours(
                new XMLDocument("<provides><type id='Φ.oak'/></provides>")
            ).all(),
            Matchers.anEmptyMap()
        );
    }
}
