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
import java.security.SecureRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
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
    void namesArgumentAfterVoidItLandsIn(@Mktmp final Path temp) throws IOException {
        final String hollow = String.format("lid%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "the argument must take the name of the void it lands in, but it didnt",
            new Landings(
                LandingsTest.tables(
                    temp,
                    LandingsTest.grounded("Φ.k.φ", "Φ.k.φ.α0"),
                    String.format(
                        "<o loc='Φ.k.φ.α0' copies='3' lands='%s'><is loc='Φ.number'/></o>",
                        hollow
                    )
                )
            ).names(),
            Matchers.hasEntry("Φ.k.φ.α0", hollow)
        );
    }

    @Test
    void leavesArgumentThatLandsNowhereUnnamed(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument whose row names no void must keep its place, but it was named",
            new Landings(
                LandingsTest.tables(
                    temp,
                    LandingsTest.grounded("Φ.q.φ", "Φ.q.φ.α1"),
                    "<o loc='Φ.q.φ.α1' copies='2'>",
                    "<set copies='1'><terminator/></set>",
                    "<set copies='1'><is loc='Φ.q.box'/></set>",
                    "</o>"
                )
            ).names(),
            Matchers.not(Matchers.hasKey("Φ.q.φ.α1"))
        );
    }

    @Test
    void tellsArgumentsOfOneCallApart(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the second argument of a call must take the name of its own void, but it didnt",
            new Landings(
                LandingsTest.tables(
                    temp,
                    LandingsTest.grounded("Φ.z.φ", "Φ.z.φ.α0", "Φ.z.φ.α1"),
                    "<o loc='Φ.z.φ' copies='1'><is loc='Φ.z.pair'/></o>",
                    "<o loc='Φ.z.φ.α0' copies='1' lands='left'><data/></o>",
                    "<o loc='Φ.z.φ.α1' copies='1' lands='a🌵14-6'><data/></o>"
                )
            ).names(),
            Matchers.hasEntry("Φ.z.φ.α1", "a🌵14-6")
        );
    }

    @Test
    void leavesArgumentOfCallOnVoidUnnamed(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument of a call on a void must keep its place, but it was named",
            new Landings(
                LandingsTest.tables(
                    temp,
                    String.join(
                        "",
                        "<facts><objects><type id='Φ.guard'>",
                        "<attr name='fail' type='Φ.guard.fail' void='true'/>",
                        "<attr name='φ' type='Φ.guard.φ'/>",
                        "</type></objects>",
                        "<reads><read id='Φ.guard.φ' name='fail' of='Φ.guard.φ.ρ'/></reads>",
                        "<heads><type id='Φ.guard.φ.ρ'><ref loc='Φ.guard'/></type></heads>",
                        "<calls><call loc='Φ.guard.φ'><arg loc='Φ.guard.φ.α0' n='0'/></call>",
                        "</calls></facts>"
                    ),
                    "<o loc='Φ.guard.φ.α0' copies='1' lands='message'><data/></o>"
                )
            ).names(),
            Matchers.not(Matchers.hasKey("Φ.guard.φ.α0"))
        );
    }

    private static Path tables(
        final Path dir, final String facts, final String... rows
    ) throws IOException {
        Files.write(dir.resolve("facts.xml"), facts.getBytes(StandardCharsets.UTF_8));
        Files.write(
            dir.resolve("answers.xml"),
            String.format("<answers>%s</answers>", String.join("", rows))
                .getBytes(StandardCharsets.UTF_8)
        );
        return dir;
    }

    private static String grounded(final String call, final String... args) {
        return String.format(
            "<facts><objects><type id='Φ.made'/></objects><reads/><heads><type id='%1$s'><ref loc='Φ.made'/></type></heads><calls><call loc='%1$s'>%2$s</call></calls></facts>",
            call,
            IntStream.range(0, args.length)
                .mapToObj(idx -> String.format("<arg loc='%s' n='%d'/>", args[idx], idx))
                .collect(Collectors.joining())
        );
    }
}
