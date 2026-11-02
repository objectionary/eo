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
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
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
        new Report(ReportTest.program(temp), ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "every file of the program must get a page of its own, but it didnt",
            Files.exists(temp.resolve("out/cup.eo.html")),
            Matchers.is(true)
        );
    }

    @Test
    void writesAPageAnyBrowserCanOpen(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a page must say what it is before anything else, but it didnt",
            Files.readString(temp.resolve("out/cup.eo.html")),
            Matchers.startsWith("<!DOCTYPE HTML><html lang=\"en\"><head><meta charset=\"utf-8\">")
        );
    }

    @Test
    void putsNoContentTypeIntoTheHead(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a page must carry no Content-Type meta, which the Python report leaves out, but it did",
            Files.readString(temp.resolve("out/cup.eo.html")),
            Matchers.not(Matchers.containsString("Content-Type"))
        );
    }

    @Test
    void endsAPageWithALineFeed(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a page must end with a line feed, as the Python report ends it, but it didnt",
            Files.readString(temp.resolve("out/cup.eo.html")),
            Matchers.endsWith("</html>".concat(String.valueOf('\n')))
        );
    }

    @Test
    void marksTheSourceAsItsAuthorWroteIt(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the name the author wrote must be marked green, but it wasnt",
            Files.readString(temp.resolve("out/cup.eo.html")),
            Matchers.containsString("<span class=\"mark named\" data-band=\"named\">lid<span")
        );
    }

    @Test
    void listsEveryPageOnTheIndex(@Mktmp final Path temp) throws IOException {
        new Report(ReportTest.program(temp), ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the index must lead to the page of every file, but it didnt",
            Files.readString(temp.resolve("out/index.html")),
            Matchers.containsString("<a href=\"cup.eo.html\">cup.eo</a>")
        );
    }

    @Test
    void linksANestedPageWithUrlSeparators(@Mktmp final Path temp) throws IOException {
        final Path xmirs = ReportTest.program(temp);
        Files.move(
            xmirs.resolve("cup.xmir"),
            Files.createDirectories(xmirs.resolve("deep")).resolve("cup.xmir")
        );
        new Report(xmirs, ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a nested page must be linked with URL separators, but it wasnt",
            Files.readString(temp.resolve("out/index.html")),
            Matchers.containsString("<a href=\"deep/cup.eo.html\">cup.eo</a>")
        );
    }

    @Test
    void writesAnIndexForAnEmptyProgram(@Mktmp final Path temp) throws IOException {
        new Report(
            Files.createDirectories(temp.resolve("xmirs")), ReportTest.answers(temp)
        ).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the index must be written even when there are no source pages, but it wasnt",
            Files.exists(temp.resolve("out/index.html")),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresADirectoryNamedLikeAnXmirFile(@Mktmp final Path temp) throws IOException {
        final Path xmirs = ReportTest.program(temp);
        Files.createDirectories(xmirs.resolve("stale.xmir"));
        new Report(xmirs, ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "a folder whose name ends with .xmir must not become a page, but it did",
            Files.exists(temp.resolve("out/stale.eo.html")),
            Matchers.is(false)
        );
    }

    @Test
    void wipesThePagesOfAnEarlierRun(@Mktmp final Path temp) throws IOException {
        final Path old = Files.createDirectories(temp.resolve("out/gone")).resolve("pot.eo.html");
        Files.writeString(old, "<html/>");
        new Report(ReportTest.program(temp), ReportTest.answers(temp)).written(temp.resolve("out"));
        MatcherAssert.assertThat(
            "the page of a source that is gone must go with it, but it stayed",
            Files.exists(old),
            Matchers.is(false)
        );
    }

    @Test
    void talliesTheObjectsOfTheProgramByBand(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the tally must count both objects of the program as named, but it didnt",
            new Report(ReportTest.program(temp), ReportTest.answers(temp))
                .written(temp.resolve("out")),
            Matchers.allOf(Matchers.hasEntry("named", 2), Matchers.hasEntry("unreached", 0))
        );
    }

    /**
     * Draws the pages of a whole run, the prepared XMIR in the folder that the
     * system property {@code report.xmirs} names and the table in the file
     * {@code report.answers} names, and compares them byte for byte with the
     * pages the Python report drew of the same run, in the folder
     * {@code report.html} names. Skipped when the properties are not set:
     * <pre>mvn test -pl eo-inference -Dtest=ReportTest \
     *   -Dreport.xmirs=pre -Dreport.answers=answers.xml -Dreport.html=html</pre>
     *
     * @param temp Temporary folder
     * @throws IOException If a file cannot be read or written
     */
    @Test
    @EnabledIfSystemProperty(named = "report.xmirs", matches = ".+")
    void drawsTheBytesThePythonReportDraws(@Mktmp final Path temp) throws IOException {
        new Report(
            Paths.get(System.getProperty("report.xmirs")),
            Paths.get(System.getProperty("report.answers"))
        ).written(temp.resolve("html"));
        MatcherAssert.assertThat(
            "the pages of the run must be the bytes the Python report drew, but these arent",
            ReportTest.differing(
                temp.resolve("html"), Paths.get(System.getProperty("report.html"))
            ),
            Matchers.empty()
        );
    }

    private static Path program(final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            String.join(
                "",
                "<object><listing>[] &gt; cup&#10;  [] &gt; lid&#10;</listing>",
                "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                "<o line='2' loc='Φ.cup.lid' name='lid' pos='2'/></o></object>"
            )
        );
        return temp.resolve("xmirs");
    }

    private static Path answers(final Path temp) throws IOException {
        Files.writeString(
            temp.resolve("answers.xml"),
            String.join(
                "",
                "<answers><o loc='Φ.cup' copies='1'><is loc='Φ.cup'/></o>",
                "<o loc='Φ.cup.lid' copies='1'><is loc='Φ.cup.lid'/></o></answers>"
            )
        );
        return temp.resolve("answers.xml");
    }

    private static Set<String> differing(final Path made, final Path drawn) throws IOException {
        final Set<String> all = new TreeSet<>(ReportTest.files(made));
        all.addAll(ReportTest.files(drawn));
        final Set<String> found = new TreeSet<>();
        for (final String file : all) {
            if (!Files.exists(made.resolve(file)) || !Files.exists(drawn.resolve(file))
                || !Arrays.equals(
                    Files.readAllBytes(made.resolve(file)), Files.readAllBytes(drawn.resolve(file))
                )) {
                found.add(file);
            }
        }
        return found;
    }

    private static Set<String> files(final Path dir) throws IOException {
        try (Stream<Path> walked = Files.walk(dir)) {
            return walked.filter(Files::isRegularFile)
                .map(path -> dir.relativize(path).toString())
                .collect(Collectors.toSet());
        }
    }
}
