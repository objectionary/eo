/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.lowering.Symbols;
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
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"λ\":\"S1\"}",
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"Δ\":\"40-08-00-00-00-00-00-00\"}",
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"λ\":\"S2\"}"
            )
        ).start();
        new Forking(new Operands(1, channel, table), table).answer();
        MatcherAssert.assertThat(
            "the fork must be spelled as its guard, both arms and the end, in order, but it wasnt",
            new String(Files.readAllBytes(file), StandardCharsets.UTF_8),
            Matchers.endsWith(
                String.join(
                    System.lineSeparator(),
                    "S3\tnumber\tfork\tsym:S1",
                    "S3\tleft",
                    "S3\tleft\tanswer\tnumber:40-08-00-00-00-00-00-00",
                    "S3\tright",
                    "S3\tright\tanswer\tsym:S2",
                    "S3\tend"
                ).concat(System.lineSeparator())
            )
        );
    }

    @Test
    void answersMarkerInCarrierOfArms(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "bool", "void", "c");
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(channel, "{\"λ\":\"S1\"}", "{\"Δ\":\"01-\"}", "{\"Δ\":\"02-\"}")
        ).start();
        MatcherAssert.assertThat(
            "the fork must answer a marker of the forma its arms carry, but it didnt",
            new Forking(new Operands(1, channel, table), table).answer(),
            Matchers.equalTo("Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ )")
        );
    }

    @Test
    void mintsFreshSymbolForEveryFork(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "bool", "void", "c");
        final Channel first = new Channel(new StringWriter());
        new Thread(
            new Oracle(first, "{\"λ\":\"S1\"}", "{\"Δ\":\"01-\"}", "{\"Δ\":\"02-\"}")
        ).start();
        new Forking(new Operands(1, first, table), table).answer();
        final Channel second = new Channel(new StringWriter());
        new Thread(
            new Oracle(second, "{\"λ\":\"S1\"}", "{\"Δ\":\"01-\"}", "{\"Δ\":\"02-\"}")
        ).start();
        MatcherAssert.assertThat(
            "a second fork over the same guard cannot share the symbol of the first, but it did",
            new Forking(new Operands(2, second, table), table).answer(),
            Matchers.equalTo("Φ.bytes( φ ↦ ⟦ λ ⤍ S3 ⟧ )")
        );
    }

    @Test
    void retypesUntypedArmToCarrierOfTheOther(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "bool", "void", "c");
        table.record("S2", "number", "void", "acc");
        table.record("S3", "object", "box", "Φ.foo.down", "acc=sym:S2");
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"λ\":\"S1\"}",
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"λ\":\"S2\"}",
                "{\"λ\":\"S3\"}"
            )
        ).start();
        new Forking(new Operands(1, channel, table), table).answer();
        MatcherAssert.assertThat(
            "the arm of no carrier must take the carrier of the other arm, but it didnt",
            table.carrier("S3"),
            Matchers.equalTo("number")
        );
    }

    @Test
    void answersCarrierOfTypedArmWhenUntypedArmComesFirst(@Mktmp final Path temp)
        throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "bool", "void", "c");
        table.record("S2", "string", "void", "txt");
        table.record("S3", "object", "box", "Φ.foo.echo", "txt=sym:S2");
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"λ\":\"S1\"}",
                "{\"λ\":\"S3\"}",
                "{\"𝑛\":\"Φ.string( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) )\",\"Φ.\":\"string\"}",
                "{\"λ\":\"S2\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "the fork must answer in the carrier of its typed arm, but it didnt",
            new Forking(new Operands(1, channel, table), table).answer(),
            Matchers.equalTo("Φ.string( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S4 ⟧ ) )")
        );
    }
}
