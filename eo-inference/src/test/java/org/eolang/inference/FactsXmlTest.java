/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link FactsXml}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class FactsXmlTest {

    @Test
    void readsVoidsInOrderTheyAreWritten(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the voids must be read in the order they are written, ρ left out, but they werent",
            new FactsXml(FactsXmlTest.facts(temp)).program().form("Φ.lamp").order(),
            Matchers.contains("wire", "bulb")
        );
    }

    @Test
    void readsTypesAtomCallsVoidBackWith(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the types an atom calls a void back with must be read apart, but they werent",
            new FactsXml(FactsXmlTest.facts(temp)).program().form("Φ.lamp").args("bulb"),
            Matchers.contains("Φ.glass", "Φ.wire")
        );
    }

    @Test
    void readsDeclaredRho(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the formation a declared ρ holds must be read, but it wasnt",
            new FactsXml(FactsXmlTest.facts(temp)).program().form("Φ.lamp").rho(),
            Matchers.equalTo("Φ.room")
        );
    }

    @Test
    void readsArgumentsOfCallByTheirNumbers(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the arguments of a call must be read in the order of their numbers, but they werent",
            new FactsXml(FactsXmlTest.facts(temp)).program().args("Φ.room.φ"),
            Matchers.contains("Φ.room.φ.α0", "Φ.room.φ.α1")
        );
    }

    @Test
    void readsWhatChainStartsFrom(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a chain that starts from a datum must say so, but it didnt",
            new FactsXml(FactsXmlTest.facts(temp)).program().heads("Φ.room.φ.α1").get(0).tag(),
            Matchers.equalTo("data")
        );
    }

    private static Path facts(final Path temp) throws IOException {
        final Path file = temp.resolve("facts.xml");
        Files.write(
            file,
            String.join(
                String.valueOf('\n'),
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<facts>",
                "  <objects>",
                "    <type bare=\"true\" complete=\"false\" id=\"Φ.lamp\" returns=\"Φ.room\">",
                "      <attr name=\"wire\" type=\"Φ.lamp.wire\" void=\"true\"/>",
                "      <attr name=\"ρ\" rho=\"Φ.room\" type=\"Φ.lamp.ρ\" void=\"true\"/>",
                "      <attr args=\"Φ.glass,Φ.wire\" name=\"bulb\" type=\"Φ.lamp.bulb\" void=\"true\"/>",
                "    </type>",
                "  </objects>",
                "  <reads>",
                "    <read id=\"Φ.room.φ\" name=\"lamp\" of=\"Φ.room.φ.ρ\"/>",
                "  </reads>",
                "  <heads>",
                "    <type id=\"Φ.room.φ.α1\">",
                "      <data/>",
                "    </type>",
                "  </heads>",
                "  <calls>",
                "    <call loc=\"Φ.room.φ\">",
                "      <arg loc=\"Φ.room.φ.α1\" n=\"1\"/>",
                "      <arg loc=\"Φ.room.φ.α0\" n=\"0\"/>",
                "    </call>",
                "  </calls>",
                "</facts>",
                ""
            ).getBytes(StandardCharsets.UTF_8)
        );
        return file;
    }
}
