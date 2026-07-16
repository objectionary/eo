/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtomTypes}.
 * @since 0.57
 */
final class AtomTypesTest {

    @Test
    void findsDeclaredTypeOfKnownAtom() {
        MatcherAssert.assertThat(
            "Table must return the forma declared for a known atom, but it didnt",
            new AtomTypes(
                Collections.singletonMap("Φ.number.plus", "Φ.number")
            ).declared("Φ.number.plus"),
            Matchers.equalTo("Φ.number")
        );
    }

    @Test
    void returnsEmptyForUndeclaredAtom() {
        MatcherAssert.assertThat(
            "Table must return empty forma for an atom that declares no type, but it didnt",
            new AtomTypes(Collections.emptyMap()).declared("Φ.stdout"),
            Matchers.equalTo("")
        );
    }
}
