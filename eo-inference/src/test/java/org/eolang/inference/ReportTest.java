/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Report}.
 *
 * @since 0.70.0
 */
@ExtendWith(MktmpResolver.class)
final class ReportTest {

    @Test
    void writesAPageForEverySourceFile(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "every file of the program must get a page of its own, but it didnt",
            new Report(ReportTest.program(temp), ReportTest.tables(temp))
                .written(temp.resolve("out")),
            Matchers.equalTo(1)
        );
    }

    @Test
    void writesAPageAnyBrowserCanOpen(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.tables(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a page must say what it is before anything else, but it didnt",
            Files.readString(temp.resolve("out").resolve("cup.eo.html")),
            Matchers.startsWithIgnoringCase("<!DOCTYPE html")
        );
    }

    @Test
    void marksTheSourceAsItsAuthorWroteIt(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.tables(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the page must show the name the author wrote, but it didnt",
            Files.readString(temp.resolve("out").resolve("cup.eo.html")),
            Matchers.containsString("lid")
        );
    }

    @Test
    void countsTheVoidsAnAtomFillsApart(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.tables(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the tally must count the voids an atom fills apart from the rest, but it didnt",
            Files.readString(temp.resolve("out").resolve("cup.eo.html")),
            Matchers.containsString("filled by an atom")
        );
    }

    @Test
    void countsTheVoidsNobodyFillsApart(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.tables(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the tally must count the voids nobody fills apart from the rest, but it didnt",
            Files.readString(temp.resolve("out").resolve("cup.eo.html")),
            Matchers.containsString("filled by nobody")
        );
    }

    @Test
    void listsEveryPageOnTheIndex(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.tables(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the index must lead to the page of every file, but it didnt",
            Files.readString(temp.resolve("out").resolve("index.html")),
            Matchers.containsString("cup.eo.html")
        );
    }

    @Test
    void linksANestedPageWithUrlSeparators(@Mktmp final Path temp) throws IOException {
        final Path xmirs = ReportTest.program(temp);
        Files.move(
            xmirs.resolve("cup.xmir"),
            Files.createDirectories(xmirs.resolve("deep")).resolve("cup.xmir")
        );
        new Report(xmirs, ReportTest.tables(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a nested page must be linked with URL separators, but it wasnt",
            Files.readString(temp.resolve("out").resolve("index.html")),
            Matchers.containsString("deep/cup.eo.html")
        );
    }

    @Test
    void writesAnIndexForAnEmptyProgram(@Mktmp final Path temp) throws IOException {
        final Path xmirs = Files.createDirectories(temp.resolve("xmirs"));
        final Path tables = temp.resolve("tables");
        new Resolved(new Clues()).follow(xmirs, tables);
        new Report(xmirs, tables).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the index must be written even when there are no source pages",
            Files.exists(temp.resolve("out").resolve("index.html")),
            Matchers.equalTo(true)
        );
    }

    @Test
    void ignoresADirectoryNamedLikeAnXmirFile(@Mktmp final Path temp) throws IOException {
        final Path program = ReportTest.program(temp);
        final Path tables = ReportTest.tables(temp);
        Files.createDirectories(program.resolve("stale.xmir"));
        MatcherAssert.assertThat(
            "a folder whose name ends with .xmir must not become a page, but it did",
            new Report(program, tables).written(temp.resolve("out")),
            Matchers.equalTo(1)
        );
    }

    @Test
    void saysNothingOfTheBytesOfALiteral(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.literal(temp), ReportTest.tables(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the page must not name the bytes of a literal after the literal, but it did",
            Files.readString(temp.resolve("out").resolve("cup.eo.html")),
            Matchers.not(Matchers.containsString("Φ.cup.lid.α0"))
        );
    }

    @Test
    void saysTooManyThingsOfALongChoice(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.crowded(temp), ReportTest.witnessed(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a choice of nine must be shown as too many things to name, but it was listed",
            Files.readString(temp.resolve("out").resolve("cup.eo.html")),
            Matchers.containsString("too many things to name")
        );
    }

    @Test
    void namesNoMemberOfALongChoice(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.crowded(temp), ReportTest.witnessed(temp))
            .written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "no member of a choice too long to print may reach the page, but one did",
            Files.readString(temp.resolve("out").resolve("cup.eo.html")),
            Matchers.not(Matchers.containsString("Φ.ivy"))
        );
    }

    private static Path program(final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            String.join(
                "",
                "<object><listing>",
                String.join(
                    System.lineSeparator(), "[] &gt; cup", "  [] &gt; lid", ""
                ),
                "</listing>",
                "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                "<o line='2' loc='Φ.cup.lid' name='lid' pos='2'/></o></object>"
            )
        );
        return temp.resolve("xmirs");
    }

    private static Path literal(final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            String.join(
                "",
                "<object><listing>",
                String.join(
                    System.lineSeparator(), "[] &gt; cup", "  cup 42 &gt; lid", ""
                ),
                "</listing>",
                "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                "<o base='Φ.cup' line='2' loc='Φ.cup.lid' name='lid' pos='2'>",
                "<o as='α0' line='2' loc='Φ.cup.lid.α0' pos='6'>2A-</o>",
                "</o></o></object>"
            )
        );
        return temp.resolve("xmirs");
    }

    private static Path tables(final Path temp) throws IOException {
        new Resolved(new Clues()).follow(temp.resolve("xmirs"), temp.resolve("tables"));
        return temp.resolve("tables");
    }

    private static Path crowded(final Path temp) throws IOException {
        final String[] trees = {
            "oak", "elm", "ash", "fir", "yew", "box", "bay", "gum", "ivy",
        };
        final StringBuilder text = new StringBuilder("[x] &gt; cup");
        final StringBuilder nodes = new StringBuilder(
            String.join(
                "",
                "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                "<o base='∅' line='1' loc='Φ.cup.x' name='x' pos='1'/></o>"
            )
        );
        for (int idx = 0; idx < trees.length; idx += 1) {
            text.append(System.lineSeparator())
                .append(String.format("cup %1$s &gt; cup-%1$s", trees[idx]));
            nodes.append(
                String.format(
                    String.join(
                        "",
                        "<o loc='Φ.%1$s' name='%1$s'/>",
                        "<o base='Φ.cup' line='%2$d' loc='Φ.cup-%1$s' name='cup-%1$s' pos='0'>",
                        "<o as='α0' base='Φ.%1$s' loc='Φ.cup-%1$s.α0'/></o>"
                    ),
                    trees[idx], idx + 2
                )
            );
        }
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            String.format(
                "<object><listing>%s%s</listing>%s</object>",
                text, System.lineSeparator(), nodes
            )
        );
        return temp.resolve("xmirs");
    }

    private static Path witnessed(final Path temp) throws IOException {
        new Witnessed(new Demanded(new Resolved(new Clues())))
            .follow(temp.resolve("xmirs"), temp.resolve("tables"));
        return temp.resolve("tables");
    }
}
