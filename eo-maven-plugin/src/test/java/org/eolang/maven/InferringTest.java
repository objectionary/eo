/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Collections;
import org.cactoos.io.Directory;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Inferring}.
 *
 * <p>What the facts say about a program is checked by the packs of
 * {@code eo-inference}. What is left here are the mechanics no EO source
 * can express: how many files there are, where they land, and what happens
 * to a file whose source is gone.</p>
 *
 * @since 0.67.0
 */
@ExtendWith(MktmpResolver.class)
final class InferringTest {

    @Test
    void putsEveryFileOfProgramInOneTable(@Mktmp final Path temp) throws IOException {
        final Path many = Files.createDirectories(temp.resolve("many"));
        Files.writeString(
            many.resolve("kettle.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > kettle", "  [] > steam", "")
            ).parsed().toString()
        );
        Files.createDirectories(many.resolve("nested"));
        Files.writeString(
            many.resolve("nested").resolve("cup.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > cup", "  [] > handle", "")
            ).parsed().toString()
        );
        new Inferring(
            new Directory(many), many, temp.resolve("all"), temp.resolve("tables"),
            new RuntimeNames().get()
        ).exec();
        MatcherAssert.assertThat(
            "one document must cover the whole program, but a file was left out of it",
            new XMLDocument(temp.resolve("tables").resolve("facts.xml")),
            XhtmlMatchers.hasXPaths(
                "/facts/objects/type[@id='Φ.kettle']/attr[@name='steam']",
                "/facts/objects/type[@id='Φ.cup']/attr[@name='handle']"
            )
        );
    }

    @Test
    void writesAnswersOfProgramBesideItsFacts(@Mktmp final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("garden"));
        Files.writeString(
            sources.resolve("pond.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > pond", "  [] > frog", "")
            ).parsed().toString()
        );
        new Inferring(
            new Directory(sources), sources, temp.resolve("pre"), temp.resolve("tables"),
            new RuntimeNames().get()
        ).exec();
        MatcherAssert.assertThat(
            "the answers must be written beside the facts and tell what the frog is, but they werent",
            new XMLDocument(temp.resolve("tables").resolve("answers.xml")),
            XhtmlMatchers.hasXPath("/answers/o[@loc='Φ.pond.frog']/is[@loc='Φ.pond.frog']")
        );
    }

    @Test
    void leavesOutAnXmirTheCallerDidNotName(@Mktmp final Path temp) throws IOException {
        final Path parsed = Files.createDirectories(temp.resolve("parsed"));
        Files.writeString(
            parsed.resolve("app.xmir"),
            new EoSyntax(String.join(System.lineSeparator(), "[] > app", "")).parsed().toString()
        );
        Files.writeString(
            parsed.resolve("bee.xmir"),
            new EoSyntax(String.join(System.lineSeparator(), "[] > bee", "")).parsed().toString()
        );
        new Inferring(
            Collections.singletonList(parsed.resolve("app.xmir")),
            parsed,
            temp.resolve("pre"),
            temp.resolve("tables"),
            new RuntimeNames().get()
        ).exec();
        MatcherAssert.assertThat(
            "a file left behind by a deleted source must stay out of the facts, but it didnt",
            new XMLDocument(temp.resolve("tables").resolve("facts.xml"))
                .nodes("/facts/objects/type[@id='Φ.bee']"),
            Matchers.empty()
        );
    }

    @Test
    void forgetsSourceThatIsGone(@Mktmp final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("shed"));
        Files.writeString(
            sources.resolve("rake.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > rake", "  [] > teeth", "")
            ).parsed().toString()
        );
        new Inferring(
            new Directory(sources), sources, temp.resolve("pre"), temp.resolve("rows"),
            new RuntimeNames().get()
        ).exec();
        Files.delete(sources.resolve("rake.xmir"));
        Files.writeString(
            sources.resolve("hoe.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > hoe", "  [] > blade", "")
            ).parsed().toString()
        );
        new Inferring(
            new Directory(sources), sources, temp.resolve("pre"), temp.resolve("rows"),
            new RuntimeNames().get()
        ).exec();
        MatcherAssert.assertThat(
            "a file whose source is gone must leave the facts, but it stayed",
            new XMLDocument(temp.resolve("rows").resolve("facts.xml")),
            XhtmlMatchers.hasXPath("/facts/objects[not(type[@id='Φ.rake'])]")
        );
    }

    @Test
    void keepsFoldersOfProgram(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("tree").resolve("one")).resolve("leaf.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > leaf", "  [] > vein", "")
            ).parsed().toString()
        );
        new Inferring(
            new Directory(temp.resolve("tree")), temp.resolve("tree"),
            temp.resolve("copy"), temp.resolve("rows"), new RuntimeNames().get()
        ).exec();
        MatcherAssert.assertThat(
            "a file in a folder must be written to the same folder again, but it wasnt",
            Files.exists(temp.resolve("copy").resolve("one").resolve("leaf.xmir")),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresADirectoryNamedLikeAnXmirFile(@Mktmp final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("yard"));
        Files.writeString(
            sources.resolve("spade.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > spade", "  [] > handle", "")
            ).parsed().toString()
        );
        Files.createDirectories(sources.resolve("stale.xmir"));
        new Inferring(
            new Directory(sources), sources, temp.resolve("pre"), temp.resolve("rows"),
            new RuntimeNames().get()
        ).exec();
        MatcherAssert.assertThat(
            "a folder whose name ends with .xmir must be left alone, but it was read",
            new XMLDocument(temp.resolve("rows").resolve("facts.xml")),
            XhtmlMatchers.hasXPath("/facts/objects/type[@id='Φ.spade']/attr[@name='handle']")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void stopsWhenPreparedXmirsSurviveTheCleanup(@Mktmp final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("yard"));
        Files.writeString(
            sources.resolve("spade.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > spade", "  [] > handle", "")
            ).parsed().toString()
        );
        final Path pre = temp.resolve("pre");
        final Path locked = Files.createDirectories(pre.resolve("locked"));
        Files.writeString(locked.resolve("old.xmir"), "<object/>");
        Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("r-xr-xr-x"));
        try {
            Assumptions.assumeTrue(
                !Files.isWritable(locked),
                "read-only directories don't stop deletions for this user (root?), can't test"
            );
            Assertions.assertThrows(
                IOException.class,
                () -> new Inferring(
                    new Directory(sources), sources, pre, temp.resolve("rows"),
                    new RuntimeNames().get()
                ).exec(),
                "a prepared XMIR of an earlier run that could not be removed must stop inference, not be read together with the current ones"
            );
        } finally {
            Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("rwxr-xr-x"));
        }
    }

    @Test
    void saysNothingWhenThereIsNothingToCleanUp(@Mktmp final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("shed"));
        Files.writeString(
            sources.resolve("rake.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > rake", "  [] > teeth", "")
            ).parsed().toString()
        );
        Assertions.assertDoesNotThrow(
            () -> new Inferring(
                new Directory(sources), sources, temp.resolve("pre"), temp.resolve("rows"),
                new RuntimeNames().get()
            ).exec(),
            "a prepared directory that was never made is not a failed cleanup, but it was taken for one"
        );
    }
}
