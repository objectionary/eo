/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Universe}.
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class UniverseTest {

    @Test
    void holdsTheMethodTables() {
        MatcherAssert.assertThat(
            "the tables of the primitives must be in the universe, but they arent",
            new Universe().text(),
            Matchers.containsString("λ ⤍ L_number_plus")
        );
    }

    @Test
    void resolvesReferenceWhenMerged(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 100, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the universe must be a complete expression phino can merge with, but it isnt",
            phino.dataize(new Universe().text(), "⟦ φ ↦ Φ.true ⟧").bytes(),
            Matchers.equalTo("01-")
        );
    }
}
