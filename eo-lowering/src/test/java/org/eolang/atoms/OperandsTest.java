/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.StringWriter;
import java.nio.file.Path;
import java.util.Arrays;
import org.eolang.lowering.Box;
import org.eolang.lowering.Symbols;
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
    void readsSymbolOffTheLambdaFact(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"λ\":\"S3\"}")).start();
        MatcherAssert.assertThat(
            "a typed operand stuck on a symbol must be read as that symbol, but it wasnt",
            new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).of("x", "number"),
            Matchers.equalTo("sym:S3")
        );
    }

    @Test
    void typesDataByTheFormaExpected(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"Δ\":\"FF-\"}")).start();
        MatcherAssert.assertThat(
            "bare bytes must take the forma the operation expects, but they didnt",
            new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).of("x", "bool"),
            Matchers.equalTo("bool:FF-")
        );
    }

    @Test
    void asksOnceWhenFormaIsKnown(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        final Channel channel = new Channel(out);
        new Thread(new Oracle(channel, "{\"Δ\":\"40-08-00-00-00-00-00-00\"}")).start();
        new Operands(4, channel, new Symbols(temp.resolve("s.tsv"))).of("x", "number");
        MatcherAssert.assertThat(
            "an operand of a known forma must cost one reduced question, but it cost more",
            out.toString().trim(),
            Matchers.equalTo("{\"id\":1000001,\"of\":4,\"attr\":\"x\",\"reduce\":true}")
        );
    }

    @Test
    void readsBoolMarkerByItsGuard(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.bool( if ↦ ⟦ guard ↦ ⟦ λ ⤍ S1 ⟧, λ ⤍ L_fork ⟧ )\",\"Φ.\":\"bool\"}",
                "{\"𝑛\":\"⟦ λ ⤍ S1 ⟧\",\"λ\":\"S1\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "a bool marker must be read back as the symbol under its guard, but it wasnt",
            new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).of("flag", "bool"),
            Matchers.equalTo("sym:S1")
        );
    }

    @Test
    void asksForTheGuardOfABoolMarkerAsWritten(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        final Channel channel = new Channel(out);
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.bool( if ↦ ⟦ guard ↦ ⟦ λ ⤍ S1 ⟧ ⟧ )\",\"Φ.\":\"bool\"}",
                "{\"λ\":\"S1\"}"
            )
        ).start();
        new Operands(6, channel, new Symbols(temp.resolve("s.tsv"))).of("flag", "");
        MatcherAssert.assertThat(
            "the symbol of a bool marker must be asked for by its path, as written, but it wasnt",
            out.toString().trim(),
            Matchers.endsWith(
                "{\"id\":1000002,\"of\":6,\"attr\":\"flag.if.guard\",\"reduce\":false}"
            )
        );
    }

    @Test
    void reducesBoolMarkerForAnOperationOnBytes(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.bool( if ↦ ⟦ guard ↦ ⟦ λ ⤍ S1 ⟧ ⟧ )\",\"Φ.\":\"bool\"}",
                "{\"λ\":\"S3\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "an operation on bytes must take the bytes a bool marker reduces to, but it didnt",
            new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).of("ρ", "bytes"),
            Matchers.equalTo("sym:S3")
        );
    }

    @Test
    void refusesBoolMarkerOfNoSymbol(@Mktmp final Path temp) {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.bool( if ↦ ⟦ guard ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ )\",\"Φ.\":\"bool\"}",
                "{\"Δ\":\"FF-\"}"
            )
        ).start();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).of("flag", "bool"),
            "a bool whose guard is no symbol must be refused, but it wasnt"
        );
    }

    @Test
    void leavesBytesWrittenAsSuchUntyped(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"Δ\":\"FF-\"}")).start();
        MatcherAssert.assertThat(
            "bytes bound as they are to an untyped void must stay bytes, but they were retyped",
            new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).of("x", "object"),
            Matchers.equalTo("bytes:FF-")
        );
    }

    @Test
    void typesLiteralByItsHead(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"Δ\":\"40-08-00-00-00-00-00-00\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "a literal fed to an untyped void must take the forma of the object it applies, but it didnt",
            new Operands(2, channel, new Symbols(temp.resolve("s.tsv"))).of("x", ""),
            Matchers.equalTo("number:40-08-00-00-00-00-00-00")
        );
    }

    @Test
    void takesTruthForItsBytes(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(channel, "{\"𝑛\":\"Φ.true\",\"Φ.\":\"true\"}", "{\"Δ\":\"FF-\"}")
        ).start();
        MatcherAssert.assertThat(
            "a truth fed to an untyped void carries its bytes and no forma, but it was typed",
            new Operands(2, channel, new Symbols(temp.resolve("s.tsv"))).of("x", "object"),
            Matchers.equalTo("bytes:FF-")
        );
    }

    @Test
    void typesLiteralFedToBytesByItsHead(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 00-00-00-00-00-00-00-00 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"Δ\":\"00-00-00-00-00-00-00-00\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "a number fed to an operation on bytes must stay a number, but it was flattened",
            new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).of("x", "bytes"),
            Matchers.equalTo("number:00-00-00-00-00-00-00-00")
        );
    }

    @Test
    void witnessesBytesForSymbolOfNoHead(@Mktmp final Path temp) throws Exception {
        final Symbols symbols = new Symbols(temp.resolve("s.tsv"));
        symbols.record("S1", "object", "box", "Φ.foo.fact");
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"𝑛\":\"ξ.ρ.a\"}", "{\"λ\":\"S1\"}")).start();
        new Operands(1, channel, symbols).of("x", "bytes");
        MatcherAssert.assertThat(
            "a symbol of no carrier fed to an operation on bytes must carry bytes, but it didnt",
            symbols.carrier("S1"),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void takesDataOfNoHeadForBytes(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(channel, "{\"𝑛\":\"ξ.ρ.a\"}", "{\"Δ\":\"2A-\"}")
        ).start();
        MatcherAssert.assertThat(
            "a datum of no known forma must come back as bytes, but it didnt",
            new Operands(2, channel, new Symbols(temp.resolve("s.tsv"))).of("x", ""),
            Matchers.equalTo("bytes:2A-")
        );
    }

    @Test
    void readsSymbolWrittenAsSuchWithoutReducing(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        final Channel channel = new Channel(out);
        new Thread(new Oracle(channel, "{\"λ\":\"S5\"}")).start();
        new Operands(3, channel, new Symbols(temp.resolve("s.tsv"))).of("guard", "");
        MatcherAssert.assertThat(
            "a symbol bound as it is must cost one question as written, but it cost more",
            out.toString().trim(),
            Matchers.equalTo("{\"id\":1000001,\"of\":3,\"attr\":\"guard\",\"reduce\":false}")
        );
    }

    @Test
    void refusesReplyThatIsNeitherSymbolNorData(@Mktmp final Path temp) {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"𝑛\":\"⟦ y ↦ ∅, φ ↦ ξ.y ⟧\"}")).start();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Operands(2, channel, new Symbols(temp.resolve("s.tsv"))).of("x", "number"),
            "a reply carrying no fact must be refused, but it wasnt"
        );
    }

    @Test
    void witnessesCarrierOfSymbolFromTheOperation(@Mktmp final Path temp) throws Exception {
        final Symbols symbols = new Symbols(temp.resolve("s.tsv"));
        symbols.record("S1", "object", "box", "Φ.foo.fact", "n=sym:S2");
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"λ\":\"S1\"}")).start();
        new Operands(4, channel, symbols).of("x", "number");
        MatcherAssert.assertThat(
            "a symbol of no carrier fed to a typed operation must take its forma, but it didnt",
            symbols.carrier("S1"),
            Matchers.equalTo("number")
        );
    }

    @Test
    void treatsVoidAsNotBound(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"∅\":true}")).start();
        MatcherAssert.assertThat(
            "a void left as it is cannot count as bound, but it did",
            new Operands(1, channel, new Symbols(temp.resolve("s.tsv"))).bound("x"),
            Matchers.is(false)
        );
    }

    @Test
    void recognizesLexicalParentAsReceiver(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"⟦ f ↦ ⟦ x ↦ ∅, λ ⤍ L_box_4 ⟧, y ↦ ∅ ⟧\"}",
                "{\"𝑛\":\"⟦ x ↦ ∅, λ ⤍ L_box_4 ⟧\",\"λ\":\"L_box_4\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "a receiver holding the box itself is its lexical parent, not a value, but it was read as one",
            new Operands(3, channel, new Symbols(temp.resolve("s.tsv"))).receiver(
                new Box(Arrays.asList("L_box_4", "Φ.foo.f", "number", "object", "x:number"))
            ),
            Matchers.is(Matchers.emptyString())
        );
    }

    @Test
    void asksForTheBoxUnderItsOwnName(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        final Channel channel = new Channel(out);
        new Thread(
            new Oracle(channel, "{\"𝑛\":\"⟦ f ↦ ⟦ λ ⤍ L_box_4 ⟧ ⟧\"}", "{\"λ\":\"L_box_4\"}")
        ).start();
        new Operands(3, channel, new Symbols(temp.resolve("s.tsv"))).receiver(
            new Box(Arrays.asList("L_box_4", "Φ.foo.f", "number", "object", ""))
        );
        MatcherAssert.assertThat(
            "the parent must be asked for the box under the name it holds it by, but it wasnt",
            out.toString().trim(),
            Matchers.endsWith("{\"id\":1000002,\"of\":3,\"attr\":\"ρ.f\",\"reduce\":false}")
        );
    }

    @Test
    void refusesReceiverThatIsNotTheParent(@Mktmp final Path temp) {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(channel, "{\"𝑛\":\"⟦ f ↦ ⟦ λ ⤍ L_box_9 ⟧ ⟧\"}", "{\"λ\":\"L_box_9\"}")
        ).start();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Operands(3, channel, new Symbols(temp.resolve("s.tsv"))).receiver(
                new Box(Arrays.asList("L_box_1", "Φ.foo.f", "number", "object", ""))
            ),
            "a receiver holding another box under the name must be refused, but it wasnt"
        );
    }

    @Test
    void readsObjectMarkerAsReceiver(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"𝑛\":\"⟦ λ ⤍ S7 ⟧\",\"λ\":\"S7\"}")).start();
        MatcherAssert.assertThat(
            "a receiver that is a marker must be read as its symbol, but it wasnt",
            new Operands(3, channel, new Symbols(temp.resolve("s.tsv"))).receiver(
                new Box(Arrays.asList("L_box_2", "Φ.foo.f", "number", "object", ""))
            ),
            Matchers.equalTo("sym:S7")
        );
    }

    @Test
    void readsTupleReceiverThroughItsLength(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "tuple", "box", "Φ.foo.items");
        table.record("S2", "number", "attr", "sym:S1", "length");
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"λ\":\"S2\"}")).start();
        MatcherAssert.assertThat(
            "a tuple receiver must be read back as the symbol its length belongs to, but it wasnt",
            new Operands(3, channel, table).receiver(
                new Box(Arrays.asList("L_box_9", "Φ.tuple.at", "object", "tuple", "i:number"))
            ),
            Matchers.equalTo("sym:S1")
        );
    }

    @Test
    void asksForTheLengthOfATupleReceiver(@Mktmp final Path temp) throws Exception {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.record("S1", "tuple", "box", "Φ.foo.items");
        table.record("S2", "number", "attr", "sym:S1", "length");
        final StringWriter out = new StringWriter();
        final Channel channel = new Channel(out);
        new Thread(new Oracle(channel, "{\"λ\":\"S2\"}")).start();
        new Operands(3, channel, table).receiver(
            new Box(Arrays.asList("L_box_9", "Φ.tuple.at", "object", "tuple", "i:number"))
        );
        MatcherAssert.assertThat(
            "the length of a tuple receiver must be asked for one attribute deep, but it wasnt",
            out.toString().trim(),
            Matchers.equalTo("{\"id\":1000001,\"of\":3,\"attr\":\"ρ.length\",\"reduce\":true}")
        );
    }

    @Test
    void readsDataReceiverByTheFormaOfTheBox(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"λ\":\"S1\"}")).start();
        MatcherAssert.assertThat(
            "a receiver the box declares as data is a value, but it was taken for the parent",
            new Operands(3, channel, new Symbols(temp.resolve("s.tsv"))).receiver(
                new Box(Arrays.asList("L_box_2", "Φ.number.lt", "bool", "number", "x:number"))
            ),
            Matchers.equalTo("sym:S1")
        );
    }

    @Test
    void typesDataReceiverByTheFormaOfTheBox(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"Δ\":\"40-08-00-00-00-00-00-00\"}")).start();
        MatcherAssert.assertThat(
            "the parent read as bare bytes must take the forma the box declares for it, but it didnt",
            new Operands(3, channel, new Symbols(temp.resolve("s.tsv"))).receiver(
                new Box(Arrays.asList("L_box_1", "Φ.number.twice", "number", "number", ""))
            ),
            Matchers.equalTo("number:40-08-00-00-00-00-00-00")
        );
    }
}
