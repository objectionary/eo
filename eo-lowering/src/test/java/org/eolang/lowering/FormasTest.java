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
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Formas}.
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class FormasTest {

    @Test
    void chasesReferenceChainToItsForma() {
        final Map<String, String> rows = new HashMap<>();
        rows.put("Φ.foo.calc.φ.ρ.ρ", "Φ.foo.calc.x");
        rows.put("Φ.foo.calc.x", "Φ.number");
        MatcherAssert.assertThat(
            "the chase must land on the number forma, but it didnt",
            new Formas(rows, Collections.emptyMap()).at("Φ.foo.calc.φ.ρ.ρ"),
            Matchers.equalTo("number")
        );
    }

    @Test
    void asksTheProvidesTableWhenTheRowIsAbsent() {
        MatcherAssert.assertThat(
            "a locator without a links row must answer from the witnessed voids, but it didnt",
            new Formas(
                Collections.emptyMap(),
                Collections.singletonMap("Φ.foo.calc.x", "bytes")
            ).at("Φ.foo.calc.x"),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void stepsIntoBodyOfFormation() {
        final Map<String, String> rows = new HashMap<>();
        rows.put("Φ.foo.f.φ.ρ", "Φ.number.lt");
        rows.put("Φ.number.lt.φ", "Φ.number.gt");
        MatcherAssert.assertThat(
            "a formation must answer what its body answers, and an atom its declared forma",
            new Formas(
                rows, Collections.emptyMap(), Collections.singletonMap("Φ.number.gt", "bool")
            ).at("Φ.foo.f.φ.ρ"),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void staysSilentWhenBodyIsUnresolved() {
        final Map<String, String> rows = new HashMap<>();
        rows.put("Φ.foo.f.φ", "Φ.number.minus");
        rows.put("Φ.number.minus.φ", "Φ.number.minus.ρ.plus");
        MatcherAssert.assertThat(
            "a body the tables do not resolve names no forma, but it did",
            new Formas(rows, Collections.emptyMap()).at("Φ.foo.f.φ"),
            Matchers.equalTo("")
        );
    }

    @Test
    void readsDeclaredFormasOfAtoms(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("atoms.xml"),
            String.join(
                "",
                "<atoms><atom loc='Φ.number.gt' forma='Φ.bool'/>",
                "<atom loc='Φ.foo.opaque' forma='Φ.foo.thing'/></atoms>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "an atom must answer the carrier forma it declares, but it doesnt",
            new Formas(temp).at("Φ.number.gt"),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void ignoresAtomOfForeignForma(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("atoms.xml"),
            "<atoms><atom loc='Φ.foo.opaque' forma='Φ.foo.thing'/></atoms>"
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "an atom declaring no carrier forma names none, but it did",
            new Formas(temp).at("Φ.foo.opaque"),
            Matchers.equalTo("")
        );
    }

    @Test
    void refusesCyclicChain() {
        final Map<String, String> rows = new HashMap<>();
        rows.put("Φ.foo.a", "Φ.foo.b");
        rows.put("Φ.foo.b", "Φ.foo.a");
        MatcherAssert.assertThat(
            "a cyclic chase cannot name a forma, but it did",
            new Formas(rows, Collections.emptyMap()).at("Φ.foo.a"),
            Matchers.equalTo("")
        );
    }

    @Test
    void chasesReferenceChainToTheStringForma() {
        MatcherAssert.assertThat(
            "a chase landing on the string forma must name it, but it didnt",
            new Formas(
                Collections.singletonMap("Φ.foo.txt", "Φ.string"),
                Collections.emptyMap()
            ).at("Φ.foo.txt"),
            Matchers.equalTo("string")
        );
    }

    @Test
    void chasesReferenceChainToTheBoolForma() {
        MatcherAssert.assertThat(
            "a chase landing on a bool state must name the bool forma, but it didnt",
            new Formas(
                Collections.singletonMap("Φ.foo.flag", "Φ.false"),
                Collections.emptyMap()
            ).at("Φ.foo.flag"),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void readsRowAroundWhitespaceText(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("links.xml"),
            "<links><type id=\"Φ.foo.calc.φ.ρ.ρ\"> <ref loc=\"Φ.number\"/> </type></links>"
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a pretty-printed row must load despite the text nodes around the ref, but it didnt",
            new Formas(temp).at("Φ.foo.calc.φ.ρ.ρ"),
            Matchers.equalTo("number")
        );
    }

    @Test
    void staysBlankWithoutTables(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a directory without tables must answer as blank, but it didnt",
            new Formas(temp.resolve("nowhere")).blank(),
            Matchers.is(true)
        );
    }

    @Test
    void witnessesSingleFormaVoid(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            String.format(
                "<provides><type id=\"Φ.foo.calc\">%s</type></provides>",
                "<attr name=\"x\" void=\"true\"> <witnessed><ref loc=\"Φ.number\"/></witnessed> </attr>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a void filled only with numbers must be witnessed as one, but it wasnt",
            new Formas(temp).given("Φ.foo.calc.x"),
            Matchers.equalTo("number")
        );
    }

    @Test
    void witnessesTupleVoid(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            String.format(
                "<provides><type id=\"Φ.foo.calc\">%s</type></provides>",
                "<attr name=\"items\" void=\"true\"> <witnessed><ref loc=\"Φ.tuple\"/></witnessed> </attr>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a void filled only with tuples must be witnessed as one, but it wasnt",
            new Formas(temp).given("Φ.foo.calc.items"),
            Matchers.equalTo("tuple")
        );
    }

    @Test
    void witnessesStringVoid(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            String.format(
                "<provides><type id=\"Φ.foo.calc\">%s</type></provides>",
                "<attr name=\"t\" void=\"true\"> <witnessed><ref loc=\"Φ.string\"/></witnessed> </attr>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a void filled only with strings must be witnessed as one, but it wasnt",
            new Formas(temp).given("Φ.foo.calc.t"),
            Matchers.equalTo("string")
        );
    }

    @Test
    void witnessesBoolVoidSeeingBothStates(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            String.format(
                "<provides><type id=\"Φ.foo.calc\"><attr name=\"f\" void=\"true\">%s%s</attr></type></provides>",
                "<witnessed><ref loc=\"Φ.true\"/></witnessed>",
                "<witnessed><ref loc=\"Φ.false\"/></witnessed>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a void filled with both bool states must be witnessed as one bool, but it wasnt",
            new Formas(temp).given("Φ.foo.calc.f"),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void refusesVoidWitnessedWithMixedFormas(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            String.format(
                "<provides><type id=\"Φ.foo.calc\"><attr name=\"x\" void=\"true\">%s%s</attr></type></provides>",
                "<witnessed><ref loc=\"Φ.number\"/></witnessed>",
                "<witnessed><ref loc=\"Φ.string\"/></witnessed>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a void seeing two formas cannot be witnessed as one, but it was",
            new Formas(temp).given("Φ.foo.calc.x"),
            Matchers.equalTo("")
        );
    }
}
