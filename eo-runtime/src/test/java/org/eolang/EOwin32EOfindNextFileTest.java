/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import org.eolang.sys.Handles;
import org.eolang.sys.win32.FindFirstFileFuncCall;
import org.eolang.sys.win32.Kernel32;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOwin32$EOfind_next_file}.
 *
 * @since 0.77.0
 */
final class EOwin32EOfindNextFileTest {

    @Test
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "a number naming no open search must be refused before the kernel sees it as a handle",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOfind_next_file(), "search", new Data.ToPhi(-42)
                    ).take("code")
                ).take(),
                "reading a search that was never started was expected to fail"
            ).getMessage(),
            Matchers.containsString("'search' attribute")
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void findsEveryNameTheDirectoryHolds(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("плюшка"), new byte[0]);
        Files.createDirectory(temp.resolve("щи"));
        MatcherAssert.assertThat(
            "the search must report both children and the two dots, and nothing else",
            EOwin32EOfindNextFileTest.searched(temp),
            Matchers.containsInAnyOrder(".", "..", "плюшка", "щи")
        );
    }

    private static Collection<String> searched(final Path path) {
        final Phi first = new FindFirstFileFuncCall(Phi.Φ.take("win32").copy()).make(
            new Data.ToPhi(String.format("%s\\*", path))
        );
        final Phi handle = new Data.ToPhi(
            new Dataized(first.take("code")).asNumber().intValue()
        );
        final Collection<String> names = new ArrayList<>(0);
        names.add(new Dataized(first.take("output")).asString());
        Phi entry = EOwin32EOfindNextFileTest.entry(handle);
        while (new Dataized(entry.take("code")).asNumber().intValue() == 0) {
            names.add(new Dataized(entry.take("name")).asString());
            entry = EOwin32EOfindNextFileTest.entry(handle);
        }
        Kernel32.INSTANCE.FindClose(
            Handles.INSTANCE.remove(
                "the search this test opened",
                new Dataized(handle).asNumber().intValue()
            )
        );
        return names;
    }

    private static Phi entry(final Phi handle) {
        return new PhApplication(new EOwin32$EOfind_next_file(), "search", handle)
            .take("called");
    }
}
