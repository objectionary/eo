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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Tuple}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class TupleTest {

    @Test
    void spellsTupleWithSymbolicParts(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "tuple", "box", "Φ.foo.items");
        MatcherAssert.assertThat(
            "the tuple must carry a symbol for each of its parts, but it doesnt",
            new Tuple("S1", table).phi(),
            Matchers.equalTo(
                "Φ.tuple( length ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) ), head ↦ ⟦ λ ⤍ S3 ⟧, tail ↦ ⟦ λ ⤍ S4 ⟧ )"
            )
        );
    }

    @Test
    void mintsAttributeRowsOfParts(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "tuple", "box", "Φ.foo.items");
        new Tuple("S1", table).phi();
        MatcherAssert.assertThat(
            "the tail of the tuple must be recorded as its attribute, but it wasnt",
            table.row("S4"),
            Matchers.contains("S4", "tuple", "attr", "sym:S1", "tail")
        );
    }
}
