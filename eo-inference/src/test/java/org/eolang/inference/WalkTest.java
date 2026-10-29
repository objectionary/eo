/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Walk}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class WalkTest {

    @Test
    void walksProgramThatLacksKindOfCanon(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a program with no money must be walked with no canon, but it wasnt",
            new Walk(
                new Sources(temp).with("till.eo", "[coins] > till", "  coins > @").facts(),
                new Names(List.of("Φ.money", "Φ.number"), List.of(), List.of())
            ).told().sets().get("Φ.till"),
            Matchers.equalTo(Map.of("Φ.till", 1))
        );
    }

    @Test
    void tellsEveryPlaceOfEveryCopyThatCounts(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a place read in two copies must be told for both, but it wasnt",
            new Walk(
                new Sources(temp)
                    .with("till.eo", "[coins] > till", "  coins > @", "  coins > held")
                    .with("shop.eo", "[] > shop", "  (till 5).held > a", "  (till 6).held > b")
                    .facts()
            ).told().sets().get("Φ.till.held"),
            Matchers.equalTo(Map.of("Φ.number", 2))
        );
    }
}
