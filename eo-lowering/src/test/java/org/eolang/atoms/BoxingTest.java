/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.StringWriter;
import java.nio.file.Path;
import java.util.Map;
import org.eolang.lowering.Box;
import org.eolang.lowering.Symbols;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Boxing}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class BoxingTest {

    @Test
    void recordsBoundVoidsWithoutParent(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "number", "void", "a");
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S1 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"λ\":\"S1\"}",
                "{\"∅\":true}"
            )
        ).start();
        new Boxing(
            new Box(
                Map.of(
                    "locator", "Φ.foo.f", "carrier", "number", "parent", "-",
                    "voids", "x:number y:bool"
                )
            ),
            new Operands(1, channel, table),
            table
        ).answer();
        MatcherAssert.assertThat(
            "the box must record the locator and the bound voids only, but it didnt",
            table.row("S2"),
            Matchers.contains("S2", "number", "box", "Φ.foo.f", "x=sym:S1")
        );
    }

    @Test
    void recordsValueOfParentTypedByItsForma(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"Δ\":\"40-08-00-00-00-00-00-00\"}")).start();
        new Boxing(
            new Box(
                Map.of(
                    "locator", "Φ.number.twice", "carrier", "number",
                    "parent", "number", "voids", ""
                )
            ),
            new Operands(1, channel, table),
            table
        ).answer();
        MatcherAssert.assertThat(
            "the parent read as bare bytes must take the forma the box declares for it, but it didnt",
            table.row("S1"),
            Matchers.contains(
                "S1", "number", "box", "Φ.number.twice", "ρ=number:40-08-00-00-00-00-00-00"
            )
        );
    }

    @Test
    void omitsLexicalParent(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"⟦ f ↦ ⟦ λ ⤍ L_box_p__foo__f ⟧, k ↦ ∅ ⟧\"}",
                "{\"𝑛\":\"⟦ λ ⤍ L_box_p__foo__f ⟧\",\"λ\":\"L_box_p__foo__f\"}"
            )
        ).start();
        new Boxing(
            new Box(
                Map.of(
                    "locator", "Φ.foo.f", "carrier", "bool", "parent", "object",
                    "voids", ""
                )
            ),
            new Operands(1, channel, table),
            table
        ).answer();
        MatcherAssert.assertThat(
            "a lexical parent is where the box lives, not an operand, but it was recorded as one",
            table.row("S1"),
            Matchers.contains("S1", "bool", "box", "Φ.foo.f")
        );
    }

    @Test
    void answersMarkerOfBoxCarrier(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"Δ\":\"40-08-00-00-00-00-00-00\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "the box must answer a marker of the carrier it declares, but it didnt",
            new Boxing(
                new Box(
                    Map.of(
                        "locator", "Φ.foo.f", "carrier", "bool", "parent", "-",
                        "voids", "x:number"
                    )
                ),
                new Operands(1, channel, table),
                table
            ).answer(),
            Matchers.equalTo(
                "Φ.bool( if ↦ ⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ λ ⤍ S1 ⟧, λ ⤍ L_fork ⟧ )"
            )
        );
    }
}
