/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.SecureRandom;
import java.util.ArrayList;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.cactoos.io.ResourceOf;
import org.cactoos.list.ListOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.Trimmed;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjLower}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class MjLowerTest {

    @Test
    void doesNothingWhenDisabled(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .with("lowering", false)
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "a disabled goal must leave no folder behind, but it made one",
            temp.resolve("target").toFile(),
            Matchers.not(FileMatchers.anExistingDirectory())
        );
    }

    @Test
    void failsWhenTheBinaryIsMissing(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must name the version the lowering is pinned to, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .with("lowering", true)
                    .with("optional", false)
                    .with("binary", temp.resolve("absent").toString())
                    .execute(MjLower.class),
                "a binary that is not there must fail the build"
            ).getCause().getCause().getMessage(),
            Matchers.containsString(MjLowerTest.pin())
        );
    }

    @Test
    void skipsWhenTheBinaryIsMissingAndThatIsAllowed(@Mktmp final Path temp)
        throws IOException {
        new FakeMaven(temp)
            .with("lowering", true)
            .with("optional", true)
            .with("binary", temp.resolve("absent").toString())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "a goal with no phino to run must skip and write nothing, but it wrote something",
            new Subdir(temp.resolve("target"), "lowering").path().toFile().list(),
            Matchers.emptyArray()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void createsItsFolderWhenPhinoReportsThePinnedVersion(@Mktmp final Path temp)
        throws IOException {
        final Path home = new Subdir(temp.resolve("target"), "lowering").path();
        new FakeMaven(temp)
            .with("lowering", true)
            .with("binary", MjLowerTest.binary(temp))
            .with("tables", MjLowerTest.tables(temp).toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must make the folder it was given, but it didnt",
            home.toFile(),
            FileMatchers.anExistingDirectory()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void addsTheAtomsItRenderedToTheSourcesMavenCompiles(@Mktmp final Path temp)
        throws IOException {
        final MavenProjectStub project = new MavenProjectStub();
        project.setCompileSourceRoots(new ArrayList<>(0));
        new FakeMaven(temp)
            .with("project", project)
            .with("lowering", true)
            .with("binary", MjLowerTest.binary(temp))
            .with("tables", MjLowerTest.tables(temp).toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must hand its atoms to javac as a source root, but it didnt",
            project.getCompileSourceRoots(),
            Matchers.hasItem(
                new Subdir(temp.resolve("target"), "lowering").path()
                    .resolve("3-atoms")
                    .toAbsolutePath()
                    .toString()
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void pointsTheTranspilerAtTheSourceItPatched(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the goal must hand the transpiler the source with the atom in it, but it didnt",
            new FakeMaven(temp)
                .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
                .execute(MjParse.class)
                .with("lowering", true)
                .with("binary", MjLowerTest.binary(temp, 0, MjLowerTest.rooted()))
                .with("tables", MjLowerTest.tables(temp).toFile())
                .execute(MjLower.class)
                .foreignTojos()
                .find("foo.x.main")
                .xmir(),
            Matchers.equalTo(
                new Subdir(temp.resolve("target"), "lowering").path()
                    .resolve("4-patched/main.xmir")
                    .toAbsolutePath()
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void ignoresTheCopyAnEarlierBuildPatched(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
            .execute(MjParse.class);
        Files.write(
            Files.createDirectories(
                new Subdir(temp.resolve("target"), "lowering").path().resolve("4-patched")
            ).resolve("main.xmir"),
            "<object/>".getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "the goal must not hand the transpiler a copy it did not patch now, but it did",
            maven
                .with("lowering", true)
                .with("binary", MjLowerTest.binary(temp))
                .with("tables", MjLowerTest.tables(temp).toFile())
                .execute(MjLower.class)
                .foreignTojos()
                .find("foo.x.main")
                .xmir(),
            Matchers.not(
                Matchers.equalTo(
                    new Subdir(temp.resolve("target"), "lowering").path()
                        .resolve("4-patched/main.xmir")
                        .toAbsolutePath()
                )
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void keepsTheProtocolOfARunOutOfTheBudgetItWasGiven(@Mktmp final Path temp)
        throws IOException {
        new FakeMaven(temp)
            .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
            .execute(MjParse.class)
            .with("lowering", true)
            .with("budget", 1)
            .with("binary", MjLowerTest.binary(temp, 1, "<protocol><morph><timeout limit=\"1\"/></morph><msec>1000</msec></protocol>"))
            .with("tables", MjLowerTest.tables(temp).toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "a run out of its budget must keep its protocol for study, but it doesnt",
            new Subdir(temp.resolve("target"), "lowering").path()
                .resolve("2-protocols/gap.xml")
                .toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void morphsNoEntryTheFilterOfTheIncludedDoesntMatch(@Mktmp final Path temp)
        throws IOException {
        new FakeMaven(temp)
            .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
            .execute(MjParse.class)
            .with("lowering", true)
            .with("only", "Φ\\.gapped")
            .with("binary", MjLowerTest.binary(temp))
            .with("tables", MjLowerTest.tables(temp).toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must not morph an entry that its filter doesnt include, but it did",
            temp.resolve("morph.txt").toFile(),
            Matchers.not(FileMatchers.anExistingFile())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void morphsNoEntryTheFilterOfTheExcludedMatches(@Mktmp final Path temp)
        throws IOException {
        new FakeMaven(temp)
            .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
            .execute(MjParse.class)
            .with("lowering", true)
            .with("never", ".*gap")
            .with("binary", MjLowerTest.binary(temp))
            .with("tables", MjLowerTest.tables(temp).toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must not morph an entry that its filter excludes, but it did",
            temp.resolve("morph.txt").toFile(),
            Matchers.not(FileMatchers.anExistingFile())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void handsPhinoTheNumberOfStepsItWasGiven(@Mktmp final Path temp) throws IOException {
        final int steps = new SecureRandom().nextInt(1000) + 1;
        new FakeMaven(temp)
            .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
            .execute(MjParse.class)
            .with("lowering", true)
            .with("steps", steps)
            .with("binary", MjLowerTest.binary(temp))
            .with("tables", MjLowerTest.tables(temp).toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must hand phino the number of steps it was given, but it didnt",
            Files.readString(temp.resolve("morph.txt"), StandardCharsets.UTF_8),
            Matchers.containsString(String.format("--max-steps=%d", steps))
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void plantsTheEntriesOfTheProgramItCompiled(@Mktmp final Path temp) throws IOException {
        final Path home = new Subdir(temp.resolve("target"), "lowering").path();
        new FakeMaven(temp)
            .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
            .execute(MjParse.class)
            .with("lowering", true)
            .with("binary", MjLowerTest.binary(temp))
            .with("tables", MjLowerTest.tables(temp).toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must plant the formations of the program it compiled, but it didnt",
            new XMLDocument(home.resolve("entries.xmir")).xpath("//o[@name='e1']/@base"),
            Matchers.contains("Φ.l🌵.mark")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void namesTheTablesItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path absent = temp.resolve("nowhere");
        MatcherAssert.assertThat(
            "the failure must name the directory the tables are missing from, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .with("lowering", true)
                    .with("binary", MjLowerTest.binary(temp))
                    .with("tables", absent.toFile())
                    .execute(MjLower.class),
                "tables that are not there must fail the build"
            ).getCause().getCause().getMessage(),
            Matchers.containsString(absent.toString())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void failsWhenTheBinaryReportsAnotherVersion(@Mktmp final Path temp) throws IOException {
        final Path binary = temp.resolve("phino");
        Files.write(
            binary, new ListOf<>("#!/bin/sh", "echo 0.0.1")
        );
        Files.setPosixFilePermissions(
            binary, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        MatcherAssert.assertThat(
            "the failure must name both the version found and the one pinned, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .with("lowering", true)
                    .with("binary", binary.toString())
                    .execute(MjLower.class),
                "a binary of another version must fail the build"
            ).getCause().getCause().getMessage(),
            Matchers.stringContainsInOrder("0.0.1", MjLowerTest.pin())
        );
    }

    private static String pin() {
        return new UncheckedText(
            new Trimmed(new TextOf(new ResourceOf("org/eolang/lowering/phino-version.txt")))
        ).asString();
    }

    private static String binary(final Path temp) throws IOException {
        return MjLowerTest.binary(temp, 0, "<protocol><morph/><msec>7</msec><firings>0</firings><fps>0</fps></protocol>");
    }

    private static String binary(final Path temp, final int code, final String protocol)
        throws IOException {
        final Path made = temp.resolve("phino");
        Files.write(
            made,
            new ListOf<>(
                "#!/bin/sh",
                "case $1 in",
                String.format("--version) echo %s;;", MjLowerTest.pin()),
                "merge) while [ $# -gt 0 ]; do [ \"$1\" = --target ] && : > \"$2\"; shift; done;;",
                String.format(
                    "morph) echo \"$@\" >> '%s'; for a; do case $a in --protocol=*) cp '%s' \"${a#--protocol=}\";; esac; done; exit %d;;",
                    temp.resolve("morph.txt"),
                    Files.write(
                        temp.resolve("protocol.xml"), protocol.getBytes(StandardCharsets.UTF_8)
                    ),
                    code
                ),
                "esac"
            )
        );
        Files.setPosixFilePermissions(made, PosixFilePermissions.fromString("rwxr-xr-x"));
        return made.toString();
    }

    private static String rooted() {
        return String.join(
            "",
            "<protocol><morph><evaluate λ='L_entry'><evaluate λ='L_number_times'>",
            "<bind meta='𝛿1.2'>40-00-00-00-00-00-00-00</bind>",
            "<bind meta='𝛿2.2'>40-08-00-00-00-00-00-00</bind>",
            "<minted symbol='𝜎9'>40-00-00-00-00-00-00-00 40-08-00-00-00-00-00-00</minted>",
            "</evaluate></evaluate><evaluate λ='L_root'>",
            "<dataize meta='𝛿1.3'>𝜎9:λ</dataize></evaluate></morph>",
            "<msec>13</msec><firings>2</firings><fps>153</fps></protocol>"
        );
    }

    private static Path tables(final Path temp) throws IOException {
        final Path made = Files.createDirectories(temp.resolve("tables"));
        Files.write(
            made.resolve("provides.xml"), "<provides/>".getBytes(StandardCharsets.UTF_8)
        );
        Files.write(
            made.resolve("links.xml"),
            "<links><type id='Φ.gap.φ'><ref loc='Φ.number'/></type></links>"
                .getBytes(StandardCharsets.UTF_8)
        );
        Files.write(made.resolve("atoms.xml"), "<atoms/>".getBytes(StandardCharsets.UTF_8));
        return made;
    }
}
