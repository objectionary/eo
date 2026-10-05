/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Relayed}.
 *
 * @since 0.73.0
 */
final class RelayedTest {

    @Test
    void namesAVoidWhoseFillingHandsAnArgumentBack() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.yes",
            List.of(
                Map.of("name", "left", "type", "Φ.yes.left", "void", "true"),
                Map.of("name", "φ", "type", "Φ.yes.left")
            )
        );
        MatcherAssert.assertThat(
            "a void filled with a formation that hands its own void back must be named, but it wasnt",
            new Relayed(
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                ),
                Map.of("Φ.choice.pick", List.of("Φ.yes")),
                Collections.emptyMap()
            ).all(),
            Matchers.contains("Φ.choice.pick")
        );
    }

    @Test
    void namesNothingForAVoidFilledWithWhatAnswersForItself() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.dial",
            List.of(
                Map.of("name", "sys", "type", "Φ.dial.sys", "void", "true"),
                Map.of("name", "φ", "type", "Φ.dial.face")
            )
        );
        MatcherAssert.assertThat(
            "a filling with a body of its own answers names, so its void must not be named, but it was",
            new Relayed(
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                ),
                Map.of("Φ.choice.pick", List.of("Φ.dial")),
                Collections.emptyMap()
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void namesNothingForAVoidOnlyOneFillingOfWhichHandsAnArgumentBack() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.yes",
            List.of(
                Map.of("name", "left", "type", "Φ.yes.left", "void", "true"),
                Map.of("name", "φ", "type", "Φ.yes.left")
            )
        );
        rows.put(
            "Φ.dial",
            List.of(
                Map.of("name", "sys", "type", "Φ.dial.sys", "void", "true"),
                Map.of("name", "φ", "type", "Φ.dial.face")
            )
        );
        MatcherAssert.assertThat(
            "one filling that answers for itself is owed names, so the void must not be named, but it was",
            new Relayed(
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                ),
                Map.of("Φ.choice.pick", List.of("Φ.yes", "Φ.dial")),
                Collections.emptyMap()
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void namesNothingForAVoidNobodyFilled() {
        MatcherAssert.assertThat(
            "a void nothing went into hands nothing back either, but it was named",
            new Relayed(
                new Provided(
                    new HashMap<>(0), Collections.emptyMap(),
                    Collections.emptyList(), Collections.emptyMap()
                ),
                Map.of("Φ.choice.pick", Collections.emptyList()),
                Collections.emptyMap()
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void namesNothingForAVoidFilledWithWhatStandsInFrontOfNothing() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.number",
            List.of(Map.of("name", "φ", "type", "Φ.number.φ", "void", "true"))
        );
        MatcherAssert.assertThat(
            "a body nobody binds is not an argument handed back, so the void must not be named, but it was",
            new Relayed(
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                ),
                Map.of("Φ.choice.pick", List.of("Φ.number")),
                Collections.emptyMap()
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void namesNothingForAVoidFilledWithWhatKeepsNamesOfItsOwn() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.directory",
            List.of(
                Map.of("name", "file", "type", "Φ.directory.file", "void", "true"),
                Map.of("name", "made", "type", "Φ.directory.made"),
                Map.of("name", "φ", "type", "Φ.directory.file")
            )
        );
        MatcherAssert.assertThat(
            "a filling that keeps a name of its own answers it, so the void must not be named, but it was",
            new Relayed(
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                ),
                Map.of("Φ.choice.pick", List.of("Φ.directory")),
                Collections.emptyMap()
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void looksUpAFillingByTheNameItGoesBy() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.yes",
            List.of(
                Map.of("name", "left", "type", "Φ.yes.left", "void", "true"),
                Map.of("name", "φ", "type", "Φ.yes.left")
            )
        );
        MatcherAssert.assertThat(
            "a copy of a formation hands back what the formation does, but the copy was left out",
            new Relayed(
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                ),
                Map.of("Φ.choice.pick", List.of("Φ.maybe")),
                Map.of("Φ.maybe", "Φ.yes")
            ).all(),
            Matchers.contains("Φ.choice.pick")
        );
    }
}
