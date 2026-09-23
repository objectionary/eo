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
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Arrays;
import org.cactoos.list.ListOf;
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
 * Test case for {@link Running}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class RunningTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void aimsTheOneRunAtTheEntriesOverTheWorld(@Mktmp final Path temp) throws IOException {
        final Path home = RunningTest.merged(temp);
        new Running(home, RunningTest.recording(temp)).exec();
        MatcherAssert.assertThat(
            "the one run must morph the entries of the world with the table, but it doesnt",
            RunningTest.text(home.resolve("protocol.xml")),
            Matchers.stringContainsInOrder(
                "morph",
                String.format("--symbolic=%s", home.resolve("atoms.yaml")),
                "--locator=Q.l🌵",
                home.resolve("world.phi").toString()
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void boundsTheRunWithTheStepsItWasGiven(@Mktmp final Path temp) throws IOException {
        final Path home = RunningTest.merged(temp);
        new Running(home, RunningTest.recording(temp), 7).exec();
        MatcherAssert.assertThat(
            "the run must stop at the step ceiling it was given, but it doesnt",
            RunningTest.text(home.resolve("protocol.xml")),
            Matchers.containsString("--max-steps=7")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void writesTheTableOfOperationsBesideTheWorld(@Mktmp final Path temp) throws IOException {
        final Path home = RunningTest.merged(temp);
        new Running(home, RunningTest.recording(temp)).exec();
        MatcherAssert.assertThat(
            "the table must name the mark the entries fire, but it doesnt",
            RunningTest.text(home.resolve("atoms.yaml")),
            Matchers.containsString("L_entry")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void quotesWhatTheBinaryPrintedWhenItRefusesTheRun(@Mktmp final Path temp)
        throws IOException {
        final Path home = RunningTest.merged(temp);
        final Phino phino = RunningTest.phino(temp, "echo 'no run for this one' >&2; exit 3");
        MatcherAssert.assertThat(
            "the failure must quote what the binary printed, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Running(home, phino).exec(),
                "a binary that exits with an error must fail the running"
            ).getMessage(),
            Matchers.containsString("no run for this one")
        );
    }

    @Test
    void failsNamingTheWorldItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp.resolve("lower"));
        MatcherAssert.assertThat(
            "the failure must name the world that is missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Running(home, new Phino("absent")).exec(),
                "a world that was not merged must fail the running"
            ).getMessage(),
            Matchers.containsString(home.resolve("world.phi").toString())
        );
    }

    @Test
    void runsTheWorldThePinnedPhinoFolds(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino("phino");
        Assumptions.assumeTrue(
            RunningTest.pinned(phino),
            "the pinned phino is not on this machine, so the world cannot be run here"
        );
        final Path gap = Files.write(
            temp.resolve("gap.xmir"),
            new EoSyntax(String.format("[a b] > gap%n  a.plus b > @%n")).parsed()
                .toString().getBytes(StandardCharsets.UTF_8)
        );
        final Path number = RunningTest.xmir(
            temp,
            "number",
            "<o name=\"φ\" base=\"∅\" loc=\"Φ.number.φ\"/>",
            "<o name=\"plus\" loc=\"Φ.number.plus\">",
            "<o name=\"b\" base=\"∅\" loc=\"Φ.number.plus.b\"/>",
            "<o name=\"λ\" loc=\"Φ.number.plus.λ\"/>",
            "</o>"
        );
        final Path bytes = RunningTest.xmir(
            temp, "bytes", "<o name=\"φ\" base=\"∅\" loc=\"Φ.bytes.φ\"/>"
        );
        final Path home = temp.resolve("lower");
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.write(
            tables.resolve("provides.xml"),
            String.join(
                "",
                "<provides><type id=\"Φ.gap\">",
                "<attr name=\"a\" type=\"Φ.gap.a\" void=\"true\" settled=\"Φ.number\"/>",
                "<attr name=\"b\" type=\"Φ.gap.b\" void=\"true\" settled=\"Φ.number\"/>",
                "</type></provides>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        new Planting(Arrays.asList(gap, number, bytes), tables, home).exec();
        new Merging(Arrays.asList(gap, number, bytes), home, phino).exec();
        new Running(home, phino).exec();
        MatcherAssert.assertThat(
            "the protocol must record the firing of the atom the body reached, but it doesnt",
            RunningTest.text(home.resolve("protocol.xml")),
            Matchers.stringContainsInOrder("L_entry", "L_number_plus", "L_root")
        );
    }

    private static boolean pinned(final Phino phino) {
        boolean found;
        try {
            found = phino.version().equals(phino.pin());
        } catch (final IOException ex) {
            found = false;
        }
        return found;
    }

    private static String text(final Path file) throws IOException {
        return new String(Files.readAllBytes(file), StandardCharsets.UTF_8);
    }

    private static Path merged(final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp.resolve("lower"));
        Files.write(home.resolve("world.phi"), "⟦ ⟧".getBytes(StandardCharsets.UTF_8));
        return home;
    }

    private static Path xmir(final Path temp, final String name, final String... body)
        throws IOException {
        return Files.write(
            temp.resolve(String.format("%s.xmir", name)),
            String.format(
                "<object><o name=\"%s\" loc=\"Φ.%s\">%s</o></object>",
                name, name, String.join("", body)
            ).getBytes(StandardCharsets.UTF_8)
        );
    }

    private static Phino recording(final Path temp) throws IOException {
        return RunningTest.phino(
            temp,
            String.join(
                " ",
                "for a; do case $a in --protocol=*) p=${a#--protocol=};; esac; done;",
                "echo \"$@\" > \"$p\""
            )
        );
    }

    private static Phino phino(final Path temp, final String body) throws IOException {
        final Path binary = Files.write(
            temp.resolve("phino"), new ListOf<>("#!/bin/sh", body)
        );
        Files.setPosixFilePermissions(binary, PosixFilePermissions.fromString("rwxr-xr-x"));
        return new Phino(binary.toString());
    }
}
