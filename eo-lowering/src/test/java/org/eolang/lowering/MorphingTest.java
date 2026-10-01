/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.list.ListOf;
import org.eolang.cache.GcShared;
import org.eolang.cache.GlobalCache;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Morphing}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class MorphingTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void writesOneProtocolPerEntryOfTheWorld(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 1, 2);
        final Path protocols = temp.resolve("2-protocols");
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        try (Stream<Path> made = Files.list(protocols)) {
            MatcherAssert.assertThat(
                "every entry must get a protocol of its own, but one is missing",
                made.map(Path::getFileName).map(Path::toString).collect(Collectors.toList()),
                Matchers.containsInAnyOrder("e1.xml", "e2.xml")
            );
        }
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void morphsOnlyTheEntriesTheFilterMatches(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 3, 71, 12);
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope("Φ\\.e7.", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        try (Stream<Path> made = Files.list(temp.resolve("2-protocols"))) {
            MatcherAssert.assertThat(
                "only the entries the filter matches must be morphed, but others were",
                made.map(Path::getFileName).map(Path::toString).collect(Collectors.toList()),
                Matchers.contains("e71.xml")
            );
        }
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void writesTheProtocolOfAnEntryUnderItsLocator(@Mktmp final Path temp)
        throws IOException {
        final Path home = Files.createDirectories(temp);
        Files.write(home.resolve("world.phi"), "⟦ ⟧".getBytes(StandardCharsets.UTF_8));
        Files.write(
            home.resolve("entries.tsv"),
            String.format("4\tΦ.bytes.as-hex.a🌵16-3%n").getBytes(StandardCharsets.UTF_8)
        );
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the protocol must be named after the locator of its entry, but it isnt",
            temp.resolve("2-protocols/bytes/as-hex/a🌵16-3.xml").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void keepsTheProtocolOfARunOutOfItsBudget(@Mktmp final Path temp)
        throws IOException {
        MorphingTest.merged(temp, 5);
        new Morphing(
            MorphingTest.phino(
                temp,
                String.join(
                    " ",
                    "for a; do case $a in --protocol=*) p=${a#--protocol=};; esac; done;",
                    "echo '<morph><timeout limit=\"1\"/></morph>' > \"$p\"; exit 1"
                )
            ),
            new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 32,
            Duration.ofMillis(900L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "a run out of its budget must keep its protocol for study, but it doesnt",
            temp.resolve("2-protocols/e5.xml").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void takesTheProtocolFromTheCacheWhenTheWorldIsTheSame(@Mktmp final Path temp)
        throws IOException {
        MorphingTest.merged(temp, 3);
        final Phino phino = MorphingTest.counting(temp);
        new Morphing(
            phino, new GcShared(temp.resolve("cache"), "0.1.2"),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        new Morphing(
            phino, new GcShared(temp.resolve("cache"), "0.1.2"),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the second build over the same world must not run the binary again, but it does",
            Files.readAllLines(temp.resolve("runs.txt")),
            Matchers.hasSize(1)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void morphsAgainWhenTheWorldChanges(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 3);
        final Phino phino = MorphingTest.counting(temp);
        new Morphing(
            phino, new GcShared(temp.resolve("cache"), "0.3.4"),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        Files.write(
            temp.resolve("world.phi"), "⟦ x ↦ ∅ ⟧".getBytes(StandardCharsets.UTF_8)
        );
        new Morphing(
            phino, new GcShared(temp.resolve("cache"), "0.3.4"),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "a build over a changed world must run the binary again, but it doesnt",
            Files.readAllLines(temp.resolve("runs.txt")),
            Matchers.hasSize(2)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void retriesARunOutOfTimeInAnEarlierBuild(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 8);
        new Morphing(
            MorphingTest.phino(
                temp,
                String.join(
                    " ",
                    "for a; do case $a in --protocol=*) p=${a#--protocol=};; esac; done;",
                    "echo '<morph><timeout limit=\"1\"/></morph>' > \"$p\"; exit 1"
                )
            ),
            new GcShared(temp.resolve("cache"), "0.5.6"),
            new Scope(".*", "(?!)"), 16,
            Duration.ofMillis(700L)
        ).exec(temp);
        new Morphing(
            MorphingTest.counting(temp), new GcShared(temp.resolve("cache"), "0.5.6"),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "a run out of time in an earlier build must be tried again, but it isnt",
            Files.readAllLines(temp.resolve("runs.txt")),
            Matchers.hasSize(1)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void dropsTheProtocolsOfAnEarlierBuild(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 2);
        final Path stale = Files.write(
            Files.createDirectories(temp.resolve("2-protocols/gone")).resolve("x.xml"),
            "<protocol/>".getBytes(StandardCharsets.UTF_8)
        );
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the protocol of an entry the world no longer has must be gone, but it stays",
            stale.toFile(),
            Matchers.not(FileMatchers.anExistingFile())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void aimsTheRunOfAnEntryAtItsMark(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 1, 2);
        final Path protocols = temp.resolve("2-protocols");
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the run of an entry must be aimed at the mark of that entry, but it isnt",
            MorphingTest.text(protocols.resolve("e2.xml")),
            Matchers.containsString("--locator=Q.l🌵.e2")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void morphsEveryEntryOverTheWorldWithTheTable(@Mktmp final Path temp) throws IOException {
        final Path home = MorphingTest.merged(temp, 1);
        final Path protocols = temp.resolve("2-protocols");
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the run must morph the entry of the world with the table, but it doesnt",
            MorphingTest.text(protocols.resolve("e1.xml")),
            Matchers.stringContainsInOrder(
                "morph",
                String.format("--symbolic=%s", home.resolve("atoms.yaml")),
                home.resolve("world.phi").toString()
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void boundsEveryRunWithTheStepsItWasGiven(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 1);
        final Path protocols = temp.resolve("2-protocols");
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 7, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the run must stop at the step ceiling it was given, but it doesnt",
            MorphingTest.text(protocols.resolve("e1.xml")),
            Matchers.containsString("--max-steps=7")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void printsEveryProtocolSweetAndWithoutRho(@Mktmp final Path temp) throws IOException {
        MorphingTest.merged(temp, 1);
        final Path protocols = temp.resolve("2-protocols");
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the run must print its protocol sweet and without any rho, but it doesnt",
            MorphingTest.text(protocols.resolve("e1.xml")),
            Matchers.stringContainsInOrder("--sweet", "--hide-rho")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void morphsTheEntriesSideBySide(@Mktmp final Path temp) throws IOException {
        Assumptions.assumeTrue(
            Runtime.getRuntime().availableProcessors() > 1,
            "this machine has one processor, so the entries cannot be morphed side by side here"
        );
        MorphingTest.merged(temp, 1, 2);
        final Path protocols = temp.resolve("2-protocols");
        new Morphing(
            MorphingTest.phino(
                temp,
                String.join(
                    " ",
                    "for a; do case $a in --protocol=*) p=${a#--protocol=};; esac; done;",
                    ": > \"$p.started\"; n=0;",
                    "while [ \"$(ls \"${p%/*}\"/*.started | wc -l)\" -lt 2 ]; do",
                    "n=$((n+1)); if [ $n -gt 50 ]; then",
                    "echo 'the other entry never started' >&2; exit 4; fi; sleep 0.1; done;",
                    "echo \"$@\" > \"$p\""
                )
            ),
            new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16,
            Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the second entry must be morphed while the first one still runs, but it waits",
            protocols.resolve("e2.xml").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void writesTheTableOfOperationsBesideTheWorld(@Mktmp final Path temp) throws IOException {
        final Path home = MorphingTest.merged(temp);
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the table must name the mark the entries fire, but it doesnt",
            MorphingTest.text(home.resolve("atoms.yaml")),
            Matchers.containsString("L_entry")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void quotesWhatTheBinaryPrintedWhenItRefusesARun(@Mktmp final Path temp)
        throws IOException {
        MorphingTest.merged(temp, 1);
        final Phino phino = MorphingTest.phino(temp, "echo 'no run for this one' >&2; exit 3");
        MatcherAssert.assertThat(
            "the failure must quote what the binary printed, but it doesnt",
            Assertions.assertThrows(
                UncheckedIOException.class,
                () -> new Morphing(
                    phino, new GlobalCache.GcFresh(),
                    new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
                ).exec(temp),
                "a binary that exits with an error must fail the morphing"
            ).getMessage(),
            Matchers.containsString("no run for this one")
        );
    }

    @Test
    void failsNamingTheWorldItCannotFind(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must name the world that is missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Morphing(
                    new Phino("absent"), new GlobalCache.GcFresh(),
                    new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
                ).exec(temp),
                "a world that was not merged must fail the morphing"
            ).getMessage(),
            Matchers.containsString(temp.resolve("world.phi").toString())
        );
    }

    @Test
    void failsNamingTheEntriesItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp);
        Files.write(home.resolve("world.phi"), "⟦ ⟧".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "the failure must name the entries that are missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Morphing(
                    new Phino("absent"), new GlobalCache.GcFresh(),
                    new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
                ).exec(temp),
                "entries that were not planted must fail the morphing"
            ).getMessage(),
            Matchers.containsString(home.resolve("entries.tsv").toString())
        );
    }

    @Test
    void morphsTheWorldThePinnedPhinoFolds(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino("phino");
        Assumptions.assumeTrue(
            MorphingTest.pinned(phino),
            "the pinned phino is not on this machine, so the world cannot be morphed here"
        );
        Files.write(
            Files.createDirectories(temp.resolve("1-planting")).resolve("gap.xmir"),
            new EoSyntax(String.format("[a b] > gap%n  a.plus b > @%n")).parsed()
                .toString().getBytes(StandardCharsets.UTF_8)
        );
        MorphingTest.xmir(
            temp,
            "number",
            "<o name=\"φ\" base=\"∅\" loc=\"Φ.number.φ\"/>",
            "<o name=\"plus\" loc=\"Φ.number.plus\">",
            "<o name=\"b\" base=\"∅\" loc=\"Φ.number.plus.b\"/>",
            "<o name=\"λ\" loc=\"Φ.number.plus.λ\"/>",
            "</o>"
        );
        MorphingTest.xmir(
            temp, "bytes", "<o name=\"φ\" base=\"∅\" loc=\"Φ.bytes.φ\"/>"
        );
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
        Files.write(
            tables.resolve("links.xml"),
            "<links><type id='Φ.gap.φ'><ref loc='Φ.number'/></type></links>"
                .getBytes(StandardCharsets.UTF_8)
        );
        Files.write(tables.resolve("atoms.xml"), "<atoms/>".getBytes(StandardCharsets.UTF_8));
        new Planting(tables).exec(temp);
        new Merging(phino).exec(temp);
        new Morphing(
            phino, new GlobalCache.GcFresh(), new Scope(".*", "(?!)"), 32, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the protocol must record the firing of the atom the body reached, but it doesnt",
            MorphingTest.text(temp.resolve("2-protocols/gap.xml")),
            Matchers.stringContainsInOrder("L_entry", "L_number_plus", "L_root")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void answersEveryAtomOfBytesInTheTable(@Mktmp final Path temp) throws IOException {
        final Path home = MorphingTest.merged(temp);
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the table must answer every atom of bytes, but it leaves one standing",
            Stream.of("and", "concat", "eq", "not", "or", "right", "size", "slice")
                .map(atom -> String.format("L_bytes_%s", atom))
                .collect(Collectors.toList()),
            Matchers.everyItem(
                Matchers.matchesPattern(
                    Files.readAllLines(home.resolve("atoms.yaml")).stream()
                        .filter(line -> line.startsWith("- λ: "))
                        .map(line -> String.format("(?:%s)", line.substring(5)))
                        .collect(Collectors.joining("|"))
                )
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void bringsTheBranchesOfAForkToOneShapeBeforeTheJoin(@Mktmp final Path temp)
        throws IOException {
        final Path home = MorphingTest.merged(temp);
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the fork must rewrite its branches before it symbolizes and joins them, but it doesnt",
            MorphingTest.text(home.resolve("atoms.yaml")),
            Matchers.stringContainsInOrder(
                "- λ: L_fork", "morph:", "rewrite:",
                "false-literal", "true-literal", "bool-without-rho", "decorated-bool",
                "symbolize:", "join:"
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void answersEveryBoolAsTheFormationWrappingIt(@Mktmp final Path temp) throws IOException {
        final Path home = MorphingTest.merged(temp);
        new Morphing(
            MorphingTest.recording(temp), new GlobalCache.GcFresh(),
            new Scope(".*", "(?!)"), 16, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "every bool the table answers must be the formation wrapping it, but one is bare",
            Files.readAllLines(home.resolve("atoms.yaml")).stream()
                .filter(line -> line.startsWith("  𝑛: ") && line.contains("Φ.bool("))
                .collect(Collectors.toList()),
            Matchers.<List<String>>allOf(
                Matchers.not(Matchers.empty()),
                Matchers.everyItem(Matchers.startsWith("  𝑛: ⟦ φ ↦ Φ.bool("))
            )
        );
    }

    @Test
    void answersTheAtomOfBytesThePinnedPhinoReaches(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino("phino");
        Assumptions.assumeTrue(
            MorphingTest.pinned(phino),
            "the pinned phino is not on this machine, so the world cannot be morphed here"
        );
        Files.write(
            Files.createDirectories(temp.resolve("1-planting")).resolve("len.xmir"),
            new EoSyntax(String.format("[a] > len%n  a.size > @%n")).parsed()
                .toString().getBytes(StandardCharsets.UTF_8)
        );
        MorphingTest.xmir(
            temp,
            "bytes",
            "<o name=\"φ\" base=\"∅\" loc=\"Φ.bytes.φ\"/>",
            "<o name=\"size\" loc=\"Φ.bytes.size\">",
            "<o name=\"λ\" loc=\"Φ.bytes.size.λ\"/>",
            "</o>"
        );
        MorphingTest.xmir(
            temp, "number", "<o name=\"φ\" base=\"∅\" loc=\"Φ.number.φ\"/>"
        );
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.write(
            tables.resolve("provides.xml"),
            String.join(
                "",
                "<provides><type id=\"Φ.len\">",
                "<attr name=\"a\" type=\"Φ.len.a\" void=\"true\" settled=\"Φ.bytes\"/>",
                "</type></provides>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        Files.write(
            tables.resolve("links.xml"),
            "<links><type id='Φ.len.φ'><ref loc='Φ.number'/></type></links>"
                .getBytes(StandardCharsets.UTF_8)
        );
        Files.write(tables.resolve("atoms.xml"), "<atoms/>".getBytes(StandardCharsets.UTF_8));
        new Planting(tables).exec(temp);
        new Merging(phino).exec(temp);
        new Morphing(
            phino, new GlobalCache.GcFresh(), new Scope(".*", "(?!)"), 32, Duration.ofMinutes(1L)
        ).exec(temp);
        MatcherAssert.assertThat(
            "the run must fire the atom of bytes the body reached, but it left it standing",
            MorphingTest.text(temp.resolve("2-protocols/len.xml")),
            Matchers.containsString("<evaluate λ=\"L_bytes_size\"")
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

    private static Path merged(final Path temp, final int... entries) throws IOException {
        final Path home = Files.createDirectories(temp);
        Files.write(home.resolve("world.phi"), "⟦ ⟧".getBytes(StandardCharsets.UTF_8));
        Files.write(
            home.resolve("entries.tsv"),
            Arrays.stream(entries)
                .mapToObj(entry -> String.format("%d\tΦ.e%d%n", entry, entry))
                .collect(Collectors.joining())
                .getBytes(StandardCharsets.UTF_8)
        );
        return home;
    }

    private static Path xmir(final Path temp, final String name, final String... body)
        throws IOException {
        return Files.write(
            Files.createDirectories(temp.resolve("1-planting"))
                .resolve(String.format("%s.xmir", name)),
            String.format(
                "<object><o name=\"%s\" loc=\"Φ.%s\">%s</o></object>",
                name, name, String.join("", body)
            ).getBytes(StandardCharsets.UTF_8)
        );
    }

    private static Phino counting(final Path temp) throws IOException {
        return MorphingTest.phino(
            temp,
            String.join(
                " ",
                "echo run >> \"${0%/*}/runs.txt\";",
                "for a; do case $a in --protocol=*) p=${a#--protocol=};; esac; done;",
                "echo \"$@\" > \"$p\""
            )
        );
    }

    private static Phino recording(final Path temp) throws IOException {
        return MorphingTest.phino(
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
