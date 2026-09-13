/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Forking}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class ForkingTest {

    @Test
    void recordsArmsInOrder(@Mktmp final Path temp) throws Exception {
        final Path file = temp.resolve("s.tsv");
        final Symbols table = new Symbols(file);
        table.record("S1", "bool", "void", "c");
        table.record("S2", "number", "void", "n");
        new Forking(
            new Operands(
                1,
                new Bindings(
                    "⟦ left ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) ), right ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) ), guard ↦ ⟦ λ ⤍ S1 ⟧ ⟧"
                ),
                new Channel(new StringWriter()),
                table
            ),
            table
        ).answer();
        MatcherAssert.assertThat(
            "the fork must be spelled as its guard, both arms and the end, in order, but it wasnt",
            new String(Files.readAllBytes(file), StandardCharsets.UTF_8),
            Matchers.endsWith(
                String.join(
                    "\n",
                    "S3\tnumber\tfork\tsym:S1",
                    "S3\tleft",
                    "S3\tleft\tanswer\tnumber:40-08-00-00-00-00-00-00",
                    "S3\tright",
                    "S3\tright\tanswer\tsym:S2",
                    "S3\tend\n"
                )
            )
        );
    }

    @Test
    void answersMarkerInCarrierOfArms(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "bool", "void", "c");
        MatcherAssert.assertThat(
            "the fork must answer a marker of the forma its arms carry, but it didnt",
            new Forking(
                new Operands(
                    1,
                    new Bindings(
                        "⟦ left ↦ ⟦ Δ ⤍ 01- ⟧, right ↦ ⟦ Δ ⤍ 02- ⟧, guard ↦ ⟦ λ ⤍ S1 ⟧ ⟧"
                    ),
                    new Channel(new StringWriter()),
                    table
                ),
                table
            ).answer(),
            Matchers.equalTo("Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ )")
        );
    }

    @Test
    void mintsFreshSymbolForEveryFork(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "bool", "void", "c");
        final String body =
            "⟦ left ↦ ⟦ Δ ⤍ 01- ⟧, right ↦ ⟦ Δ ⤍ 02- ⟧, guard ↦ ⟦ λ ⤍ S1 ⟧ ⟧";
        new Forking(
            new Operands(1, new Bindings(body), new Channel(new StringWriter()), table), table
        ).answer();
        MatcherAssert.assertThat(
            "a second fork over the same guard cannot share the symbol of the first, but it did",
            new Forking(
                new Operands(2, new Bindings(body), new Channel(new StringWriter()), table), table
            ).answer(),
            Matchers.equalTo("Φ.bytes( φ ↦ ⟦ λ ⤍ S3 ⟧ )")
        );
    }
}
