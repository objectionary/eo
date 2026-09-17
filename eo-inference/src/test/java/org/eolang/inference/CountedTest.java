/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Counted}.
 *
 * @since 0.73.0
 */
final class CountedTest {

    @Test
    void countsTwoNamesOfOneTypeOnce() {
        final Map<String, Type> told = new LinkedHashMap<>(0);
        told.put("Φ.oak", new Ref("Φ.oak"));
        told.put("Φ.alias", new Ref("Φ.alias"));
        MatcherAssert.assertThat(
            "an alias that behaves as an oak is the oak, but it was counted apart from it",
            new Counted(told, Collections.singletonMap("Φ.alias", "Φ.oak")).all(),
            Matchers.hasSize(1)
        );
    }

    @Test
    void keepsTheObjectTheWalkArrivedAt() {
        MatcherAssert.assertThat(
            "a filling must stay the object it arrived as, but it was traded for a name",
            new Counted(
                Collections.singletonMap("Φ.alias", new Ref("Φ.alias")),
                Collections.singletonMap("Φ.alias", "Φ.oak")
            ).all().iterator().next().names(),
            Matchers.equalTo("Φ.alias")
        );
    }

    @Test
    void countsTwoTypesApart() {
        final Map<String, Type> told = new LinkedHashMap<>(0);
        told.put("Φ.oak", new Ref("Φ.oak"));
        told.put("Φ.elm", new Ref("Φ.elm"));
        MatcherAssert.assertThat(
            "an oak and an elm are two things, but they were counted as one",
            new Counted(told, Collections.emptyMap()).all(),
            Matchers.hasSize(2)
        );
    }
}
