/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Rows}.
 * @since 0.71.0
 */
final class RowsTest {

    @Test
    void walksEveryRowOfATable() {
        MatcherAssert.assertThat(
            "every row of the table must come back in the order it was written, but it didnt",
            new Rows(
                new XMLDocument("<links><type id='Φ.q'/><type id='Φ.ω'/></links>")
            ).all().stream().map(row -> new Noted(row).says("id")).collect(Collectors.toList()),
            Matchers.contains("Φ.q", "Φ.ω")
        );
    }

    @Test
    void passesOverWhatIsNoRow() {
        MatcherAssert.assertThat(
            "an element that is not a type must be passed over, but it wasnt",
            new Rows(
                new XMLDocument("<provides><type id='Φ.k'/><told when='7'/></provides>")
            ).all(),
            Matchers.hasSize(1)
        );
    }

    @Test
    void findsNothingInAnEmptyTable() {
        MatcherAssert.assertThat(
            "a table with no rows must come back with nothing, but it didnt",
            new Rows(new XMLDocument("<needs/>")).all(),
            Matchers.empty()
        );
    }
}
