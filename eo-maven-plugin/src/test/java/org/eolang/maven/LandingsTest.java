/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
 * Test cases for {@link Landings}.
 *
 * @since 0.69.0
 */
@ExtendWith(MktmpResolver.class)
final class LandingsTest {

    @Test
    void namesArgumentAfterVoidOfCopiedFormation(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the argument must take the name of the void it lands in, but it didnt",
            new Landings(
                LandingsTest.table(
                    temp,
                    "<type id='Φ.k.φ'><ref loc='Φ.k.jar'>",
                    "<bind void='Φ.k.jar.lid'><ref loc='Φ.k.φ.α0'/></bind>",
                    "</ref></type>"
                )
            ).names(),
            Matchers.hasEntry("Φ.k.φ.α0", "lid")
        );
    }

    @Test
    void skipsWitnessedBind(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a bind put there only by what a void was seen to hold must not name the argument, but it did",
            new Landings(
                LandingsTest.table(
                    temp,
                    "<type id='Φ.q.φ'><ref loc='Φ.q.box'>",
                    "<bind void='Φ.q.box.pin' witnessed='true'><ref loc='Φ.q.φ.α0'/></bind>",
                    "</ref></type>"
                )
            ).names(),
            Matchers.not(Matchers.hasKey("Φ.q.φ.α0"))
        );
    }

    @Test
    void skipsBindOfWitnessedCopy(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy reached through what a void was seen to hold must not name its arguments, but it did",
            new Landings(
                LandingsTest.table(
                    temp,
                    "<type id='Φ.w.φ'><ref loc='Φ.w.refused' witnessed='true'>",
                    "<bind void='Φ.w.refused.message'><ref loc='Φ.w.φ.α0'/></bind>",
                    "</ref></type>"
                )
            ).names(),
            Matchers.not(Matchers.hasKey("Φ.w.φ.α0"))
        );
    }

    @Test
    void skipsBindReachedThroughWitnessedCopy(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument must not be named when its chain of copies passes through a witnessed one, but it was",
            new Landings(
                LandingsTest.table(
                    temp,
                    "<type id='Φ.z.half'><ref loc='Φ.z.pair' witnessed='true'/></type>",
                    "<type id='Φ.z.φ'><ref loc='Φ.z.half'>",
                    "<bind void='Φ.z.pair.right'><ref loc='Φ.z.φ.α0'/></bind>",
                    "</ref></type>"
                )
            ).names(),
            Matchers.not(Matchers.hasKey("Φ.z.φ.α0"))
        );
    }

    private static Path table(final Path dir, final String... rows) throws IOException {
        final Path links = dir.resolve("links.xml");
        Files.write(
            links,
            String.format("<links>%s</links>", String.join("", rows))
                .getBytes(StandardCharsets.UTF_8)
        );
        return links;
    }
}
