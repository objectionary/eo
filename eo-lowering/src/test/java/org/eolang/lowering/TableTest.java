/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Table}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class TableTest {

    @Test
    void readsVoidAsReference(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void symbol must be a reference to its path, but it isnt",
            TableTest.table(temp, "S1\tnumber\tvoid\tρ.a").reference("S1"),
            Matchers.equalTo("ρ.a")
        );
    }

    @Test
    void readsAttributeOfVoidAsReference(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an attribute of a void must be a reference through it, but it isnt",
            TableTest.table(
                temp, "S1\ttuple\tvoid\tt", "S2\tnumber\tattr\tsym:S1\tlength"
            ).reference("S2"),
            Matchers.equalTo("t.length")
        );
    }

    @Test
    void answersNoReferenceForComputation(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a computed symbol is no reference, but it was one",
            TableTest.table(
                temp, "S1\tnumber\tvoid\ta", "S2\tnumber\tL_number_plus\tsym:S1\tsym:S1"
            ).reference("S2"),
            Matchers.equalTo("")
        );
    }

    @Test
    void namesTupleOfPart(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the part of a tuple must name the tuple it is read off, but it doesnt",
            TableTest.table(
                temp, "S1\ttuple\tvoid\tt", "S2\tobject\tattr\tsym:S1\thead"
            ).receiver("S2"),
            Matchers.equalTo("S1")
        );
    }

    @Test
    void listsInputsInOrderOfFirstRead(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the inputs must come in the order the program reads them, but they dont",
            TableTest.table(
                temp, "S1\tnumber\tvoid\tb", "S2\tnumber\tvoid\ta",
                "S3\tnumber\tL_number_plus\tsym:S2\tsym:S1"
            ).inputs("S3").keySet(),
            Matchers.contains("a", "b")
        );
    }

    @Test
    void takesLexicalBoxAsFormationInput(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a box entered where it stands must be an input of forma formation, but it isnt",
            TableTest.table(
                temp, "S1\tnumber\tvoid\ta", "S2\tnumber\tbox\tΦ.foo.g\ty=sym:S1"
            ).inputs("S2"),
            Matchers.hasEntry("box:Φ.foo.g", "formation")
        );
    }

    @Test
    void entersBoxWithNamedBinds(@Mktmp final Path temp) throws IOException {
        final Map<String, String> names = new HashMap<>(2);
        names.put("box:Φ.foo.g", "v0");
        names.put("a", "v1");
        MatcherAssert.assertThat(
            "an entry must bind the voids of the box by name, but it doesnt",
            new JavaAtom(
                TableTest.table(
                    temp, "S1\tnumber\tvoid\ta", "S2\tnumber\tbox\tΦ.foo.g\ty=sym:S1"
                ).program("S2", names)
            ).text(),
            Matchers.containsString(
                "final double s2 = new Dataized(new PhApplication(this.take(\"v0\"), new Bind(\"y\", this.take(\"v1\")))).asNumber();"
            )
        );
    }

    @Test
    void dispatchesBoxOnItsReceiverValue(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a box with a receiver value must be dispatched on it, but it isnt",
            new JavaAtom(
                TableTest.table(
                    temp, "S1\tnumber\tvoid\ta",
                    "S2\tbool\tbox\tΦ.number.gt\tρ=sym:S1\tx=number:40-14-00-00-00-00-00-00"
                ).program("S2", Collections.singletonMap("a", "a"))
            ).text(),
            Matchers.containsString(
                "new PhApplication(new PhDispatch(this.take(\"a\"), \"gt\"), new Bind(\"x\", new Data.ToPhi(Double.longBitsToDouble(0x4014000000000000L))))"
            )
        );
    }

    @Test
    void computesArmInItsOwnFrame(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a symbol one arm alone reads must be computed inside that arm, but it isnt",
            TableTest.labels(
                TableTest.forked(temp).program("S4", Collections.singletonMap("a", "a"))
                    .bodies().get(0).protocol().moves().get(1).branches().get(0)
            ),
            Matchers.contains("s3")
        );
    }

    @Test
    void recomputesSymbolAnArmSharesWithTheTail(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a symbol an arm shares with the code after the fork must be computed there too, but it isnt",
            TableTest.labels(
                TableTest.table(
                    temp, "S1\tnumber\tvoid\ta",
                    "S2\tbool\tL_number_gt\tsym:S1\tnumber:00-00-00-00-00-00-00-00",
                    "S3\tnumber\tL_number_times\tsym:S1\tnumber:40-00-00-00-00-00-00-00",
                    "S4\tnumber\tfork\tsym:S2", "S4\tleft", "S4\tleft\tanswer\tsym:S3",
                    "S4\tright", "S4\tright\tanswer\tsym:S1", "S4\tend",
                    "S5\tnumber\tL_number_plus\tsym:S4\tsym:S3"
                ).program("S5", Collections.singletonMap("a", "a")).bodies().get(0).protocol()
            ),
            Matchers.contains("s2", "s4", "s3", "s5")
        );
    }

    @Test
    void skipsSymbolComputedAboveTheFork(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a symbol the guard computed must not be computed again in an arm, but it is",
            TableTest.labels(
                TableTest.table(
                    temp, "S1\tnumber\tvoid\ta",
                    "S3\tnumber\tL_number_times\tsym:S1\tnumber:40-00-00-00-00-00-00-00",
                    "S2\tbool\tL_number_gt\tsym:S3\tnumber:00-00-00-00-00-00-00-00",
                    "S4\tnumber\tfork\tsym:S2", "S4\tleft", "S4\tleft\tanswer\tsym:S3",
                    "S4\tright", "S4\tright\tanswer\tsym:S1", "S4\tend"
                ).program("S4", Collections.singletonMap("a", "a"))
                    .bodies().get(0).protocol().moves().get(2).branches().get(0)
            ),
            Matchers.empty()
        );
    }

    @Test
    void rendersForkIntoJava(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the fork must render into an if over the guard, but it doesnt",
            new JavaAtom(TableTest.forked(temp).program("S4", Collections.singletonMap("a", "a")))
                .text(),
            Matchers.containsString("if (s2) {")
        );
    }

    @Test
    void refusesForkWithoutAnswer(@Mktmp final Path temp) throws IOException {
        final Table table = TableTest.table(
            temp, "S1\tnumber\tvoid\ta",
            "S2\tbool\tL_number_gt\tsym:S1\tnumber:00-00-00-00-00-00-00-00",
            "S4\tnumber\tfork\tsym:S2", "S4\tleft", "S4\tleft\tanswer\tsym:S1", "S4\tright"
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> table.program("S4", Collections.singletonMap("a", "a")),
            "a fork whose arm never answered cannot be a program, but it was"
        );
    }

    @Test
    void refusesUnnamedInput(@Mktmp final Path temp) throws IOException {
        final Table table = TableTest.table(temp, "S1\tnumber\tvoid\ta");
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> table.program("S1", Collections.emptyMap()),
            "an input without a Java name cannot be read, but it was"
        );
    }

    private static Table forked(final Path temp) throws IOException {
        return TableTest.table(
            temp, "S1\tnumber\tvoid\ta",
            "S2\tbool\tL_number_gt\tsym:S1\tnumber:00-00-00-00-00-00-00-00",
            "S3\tnumber\tL_number_times\tsym:S1\tnumber:40-00-00-00-00-00-00-00",
            "S4\tnumber\tfork\tsym:S2", "S4\tleft", "S4\tleft\tanswer\tsym:S3",
            "S4\tright", "S4\tright\tanswer\tsym:S1", "S4\tend"
        );
    }

    private static List<String> labels(final Protocol proto) {
        return proto.moves().stream().map(Step::label).collect(Collectors.toList());
    }

    private static Table table(final Path temp, final String... rows) throws IOException {
        final Path file = Files.createTempFile(temp, "s", ".tsv");
        Files.write(
            file, String.join("\n", rows).concat("\n").getBytes(StandardCharsets.UTF_8)
        );
        return new Table(new Symbols(file));
    }
}
