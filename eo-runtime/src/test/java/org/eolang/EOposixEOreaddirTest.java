/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.TimeUnit;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOposix$EOreaddir}.
 *
 * @since 0.77.0
 */
final class EOposixEOreaddirTest {

    @Test
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "a number naming no open stream must be refused before it reaches libc as an address",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(new EOposix$EOreaddir(), "dirp", new Data.ToPhi(-42))
                        .take("code")
                ).take(),
                "reading a stream that was never opened was expected to fail"
            ).getMessage(),
            Matchers.containsString("'dirp' attribute")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void readsEveryNameTheDirectoryHolds(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("плюшка"), new byte[0]);
        Files.createDirectory(temp.resolve("щи"));
        MatcherAssert.assertThat(
            "the stream must report both children and the two dots, and nothing else",
            EOposixEOreaddirTest.named(temp)
                .stream()
                .map(bytes -> new String(bytes, StandardCharsets.UTF_8))
                .toList(),
            Matchers.containsInAnyOrder(".", "..", "плюшка", "щи")
        );
    }

    @Test
    @DisabledOnOs({OS.WINDOWS, OS.MAC})
    void keepsTheBytesOfANameThatIsNoText(@TempDir final Path temp) throws Exception {
        Assumptions.assumeTrue(
            EOposixEOreaddirTest.touched(temp),
            "a file whose name is no UTF-8 could not be made here"
        );
        MatcherAssert.assertThat(
            "a name kept as bytes must come back as those bytes, but it came back decoded",
            EOposixEOreaddirTest.named(temp),
            Matchers.hasItem(
                new byte[] {
                    (byte) 0xFF, (byte) 0xFE, (byte) '.', (byte) 't', (byte) 'x', (byte) 't',
                }
            )
        );
    }

    private static boolean touched(final Path dir) throws Exception {
        final Process shell = new ProcessBuilder(
            "/bin/sh", "-c", "touch \"$1/$(printf '\\377\\376').txt\"", "sh", dir.toString()
        ).start();
        try {
            return shell.waitFor(1L, TimeUnit.MINUTES) && shell.exitValue() == 0;
        } finally {
            shell.destroy();
        }
    }

    private static Collection<byte[]> named(final Path path) {
        final Phi handle = new Data.ToPhi(
            new Dataized(
                new PhApplication(
                    new EOposix$EOopendir(), "path", new Data.ToPhi(path.toString())
                ).take("code")
            ).asNumber().intValue()
        );
        final Collection<byte[]> names = new ArrayList<>(0);
        Phi entry = EOposixEOreaddirTest.entry(handle);
        while (new Dataized(entry.take("code")).asNumber().intValue() == 0) {
            names.add(new Dataized(entry.take("name")).take());
            entry = EOposixEOreaddirTest.entry(handle);
        }
        new Dataized(
            new PhApplication(new EOposix$EOclosedir(), "dirp", handle).take("code")
        ).take();
        return names;
    }

    private static Phi entry(final Phi handle) {
        return new PhApplication(new EOposix$EOreaddir(), "dirp", handle).take("called");
    }
}
