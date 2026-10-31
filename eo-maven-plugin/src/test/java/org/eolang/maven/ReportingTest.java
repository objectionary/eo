/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
 * Test case for {@link Reporting}.
 *
 * @since 0.71.0
 */
@ExtendWith(MktmpResolver.class)
final class ReportingTest {

    @Test
    void drawsTheIndexOfTheProgram(@Mktmp final Path temp) throws IOException {
        new Reporting(
            ReportingTest.prepared(temp), ReportingTest.tables(temp), temp.resolve("pages")
        ).exec();
        MatcherAssert.assertThat(
            "the index of the pages must be drawn, but it wasnt",
            Files.exists(temp.resolve("pages/index.html")),
            Matchers.is(true)
        );
    }

    @Test
    void writesTheLadderBesideTheTables(@Mktmp final Path temp) throws IOException {
        new Reporting(
            ReportingTest.prepared(temp), ReportingTest.tables(temp), temp.resolve("pages")
        ).exec();
        MatcherAssert.assertThat(
            "the numbers of the report must be written beside the tables, but they werent",
            Files.readAllLines(temp.resolve("ladder.txt")),
            Matchers.hasItem("1 objects in named")
        );
    }

    @Test
    void skipsTablesWithoutAnswers(@Mktmp final Path temp) throws IOException {
        new Reporting(
            ReportingTest.prepared(temp),
            Files.createDirectories(temp.resolve("facts-only")),
            temp.resolve("pages")
        ).exec();
        MatcherAssert.assertThat(
            "a report with no answers.xml must create no pages, but it did",
            Files.exists(temp.resolve("pages")),
            Matchers.is(false)
        );
    }

    @Test
    void skipsMissingPreparedXmirs(@Mktmp final Path temp) throws IOException {
        new Reporting(
            temp.resolve("missing"), ReportingTest.tables(temp), temp.resolve("pages")
        ).exec();
        MatcherAssert.assertThat(
            "a report with no prepared XMIR must create no pages, but it did",
            Files.exists(temp.resolve("pages")),
            Matchers.is(false)
        );
    }

    private static Path prepared(final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("pre")).resolve("pan.xmir"),
            String.join(
                "",
                "<object><listing>[] &gt; pan&#10;</listing>",
                "<o line='1' loc='Φ.pan' name='pan' pos='0'/></object>"
            )
        );
        return temp.resolve("pre");
    }

    private static Path tables(final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("tables")).resolve("answers.xml"),
            "<answers><o loc='Φ.pan' copies='2'><is loc='Φ.pan'/></o></answers>"
        );
        return temp.resolve("tables");
    }
}
