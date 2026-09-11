/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.StringWriter;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Operands}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class OperandsTest {

    @Test
    void readsBoundMarkerWithoutAsking(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a marker bound in the body must be read at once, but it wasnt",
            new Operands(
                1,
                new Bindings("⟦ x ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S3 ⟧ ) ) ⟧"),
                new Channel(new StringWriter()),
                new Symbols(temp.resolve("s.tsv"))
            ).of("x", "number"),
            Matchers.equalTo("sym:S3")
        );
    }

    @Test
    void retypesUntypedBytesToExpectedForma(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "bare bytes must take the forma the operation expects, but they didnt",
            new Operands(
                1, new Bindings("⟦ x ↦ ⟦ Δ ⤍ FF- ⟧ ⟧"),
                new Channel(new StringWriter()), new Symbols(temp.resolve("s.tsv"))
            ).of("x", "bool"),
            Matchers.equalTo("bool:FF-")
        );
    }

    @Test
    void leavesBytesUntypedForUntypedVoid(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "bare bytes bound to an untyped void must stay bytes, but they were retyped",
            new Operands(
                1, new Bindings("⟦ x ↦ ⟦ Δ ⤍ FF- ⟧ ⟧"),
                new Channel(new StringWriter()), new Symbols(temp.resolve("s.tsv"))
            ).of("x", "object"),
            Matchers.equalTo("bytes:FF-")
        );
    }

    @Test
    void asksWhenOperandIsNotReadable(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(channel, "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )")
        ).start();
        MatcherAssert.assertThat(
            "an unreadable operand must be asked for and read from the reply, but it wasnt",
            new Operands(
                2, new Bindings("⟦ x ↦ ξ.ρ.a ⟧"), channel, new Symbols(temp.resolve("s.tsv"))
            ).of("x", "number"),
            Matchers.equalTo("number:40-08-00-00-00-00-00-00")
        );
    }

    @Test
    void refusesReplyThatIsNeitherSymbolNorData(@Mktmp final Path temp) {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "⟦ y ↦ ∅, φ ↦ ξ.y ⟧")).start();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Operands(
                2, new Bindings("⟦ x ↦ ξ.ρ.a ⟧"), channel, new Symbols(temp.resolve("s.tsv"))
            ).of("x", "number"),
            "a reply carrying no data must be refused, but it wasnt"
        );
    }

    @Test
    void recognizesLexicalParentAsReceiver(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "⟦ f ↦ ⟦ x ↦ ∅, λ ⤍ L_box_4 ⟧, y ↦ ∅ ⟧")).start();
        MatcherAssert.assertThat(
            "a receiver holding the box itself is its lexical parent, not a value, but it was read as one",
            new Operands(
                3, new Bindings("⟦ x ↦ ∅ ⟧"), channel, new Symbols(temp.resolve("s.tsv"))
            ).receiver("L_box_4"),
            Matchers.is(Matchers.emptyString())
        );
    }

    @Test
    void readsTupleReceiverThroughItsLength(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "tuple", "box", "Φ.foo.items");
        table.record("S2", "number", "attr", "sym:S1", "length");
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "⟦ tail ↦ ⟦ λ ⤍ S4 ⟧, head ↦ ⟦ λ ⤍ S3 ⟧, length ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) ), at ↦ ⟦ i ↦ ∅ ⟧ ⟧"
            )
        ).start();
        MatcherAssert.assertThat(
            "a tuple receiver must be read back as the symbol its length belongs to, but it wasnt",
            new Operands(3, new Bindings("⟦ ⟧"), channel, table).receiver("L_box_9"),
            Matchers.equalTo("sym:S1")
        );
    }

    @Test
    void refusesUnreadableReceiver(@Mktmp final Path temp) {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "⟦ q ↦ ∅, φ ↦ ξ.q ⟧")).start();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Operands(
                3, new Bindings("⟦ ⟧"), channel, new Symbols(temp.resolve("s.tsv"))
            ).receiver("L_box_1"),
            "a receiver that is neither a value nor the parent must be refused, but it wasnt"
        );
    }

    @Test
    void treatsUnboundVoidAsNotBound(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a void left as ∅ cannot count as bound, but it did",
            new Operands(
                1, new Bindings("⟦ x ↦ ∅ ⟧"),
                new Channel(new StringWriter()), new Symbols(temp.resolve("s.tsv"))
            ).bound("x"),
            Matchers.is(false)
        );
    }


    @Test
    void readsDataReceiverThatCarriesTheBox(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "⟦ φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S1 ⟧ ), lt ↦ ⟦ ρ ↦ ∅, x ↦ ∅, λ ⤍ L_box_2 ⟧ ⟧"
            )
        ).start();
        MatcherAssert.assertThat(
            "a data receiver whose methods hold the box is a value, but it was taken for the parent",
            new Operands(
                3, new Bindings("⟦ x ↦ ∅ ⟧"), channel, new Symbols(temp.resolve("s.tsv"))
            ).receiver("L_box_2"),
            Matchers.equalTo("sym:S1")
        );
    }
}
