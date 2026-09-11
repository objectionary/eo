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
 * Test case for {@link Primitive}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class PrimitiveTest {

    @Test
    void foldsLiteralOperandsWithoutMinting(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "two literals must fold to data instead of a symbol, but they didnt",
            new Primitive(
                new Op("L_number_plus"),
                new Operands(
                    1,
                    new Bindings(
                        "⟦ ρ ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ) ), x ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) ) ⟧"
                    ),
                    new Channel(new StringWriter()),
                    new Symbols(temp.resolve("s.tsv"))
                ),
                new Symbols(temp.resolve("s.tsv"))
            ).answer(),
            Matchers.equalTo("Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-14-00-00-00-00-00-00 ⟧ ) )")
        );
    }

    @Test
    void mintsRowForSymbolicOperand(@Mktmp final Path temp) throws Exception {
        final Path file = temp.resolve("s.tsv");
        final Symbols table = new Symbols(file);
        table.record("S1", "number", "void", "a");
        new Primitive(
            new Op("L_number_gt"),
            new Operands(
                1,
                new Bindings(
                    "⟦ ρ ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S1 ⟧ ) ), x ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) ) ⟧"
                ),
                new Channel(new StringWriter()),
                table
            ),
            table
        ).answer();
        MatcherAssert.assertThat(
            "the operation over a symbol must land as a row of its forma, but it didnt",
            new String(Files.readAllBytes(file), StandardCharsets.UTF_8),
            Matchers.endsWith("S2\tbool\tL_number_gt\tsym:S1\tnumber:40-08-00-00-00-00-00-00\n")
        );
    }

    @Test
    void answersMarkerOfMintedSymbol(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "bytes", "void", "b");
        MatcherAssert.assertThat(
            "the answer must be the marker of the minted symbol in the forma of the operation, but it wasnt",
            new Primitive(
                new Op("L_bytes_size"),
                new Operands(
                    1, new Bindings("⟦ ρ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S1 ⟧ ) ⟧"),
                    new Channel(new StringWriter()), table
                ),
                table
            ).answer(),
            Matchers.equalTo("Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) )")
        );
    }
}
