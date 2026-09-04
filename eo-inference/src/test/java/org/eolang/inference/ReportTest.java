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

    private static Path tables(final Path temp) throws IOException {
        new Resolved(new Clues()).follow(temp.resolve("xmirs"), temp.resolve("tables"));
        return temp.resolve("tables");
    }
}
