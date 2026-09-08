/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link FindNextFileFuncCall}.
 * @since 0.76.0
 */
final class FindNextFileFuncCallTest {

    @Test
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "a number naming no open search must be refused before it reaches the kernel as a handle",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new FindNextFileFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(-42)
                ),
                "reading a search that was never started was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("'search' argument of FindNextFile")
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void findsEveryNameTheDirectoryHolds(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("плюшка"), new byte[0]);
        Files.createDirectory(temp.resolve("щи"));
        MatcherAssert.assertThat(
            "the search must report both children and the two dots, and nothing else",
            FindNextFileFuncCallTest.searched(temp),
            Matchers.containsInAnyOrder(".", "..", "плюшка", "щи")
        );
    }

    private static Collection<String> searched(final Path path) {
        final Phi win = Phi.Φ.take("win32").copy();
        final Phi first = new FindFirstFileFuncCall(win).make(
            new Data.ToPhi(String.format("%s\\*", path))
        );
        final Phi handle = new Data.ToPhi(
            new Dataized(first.take("code")).asNumber().intValue()
        );
        final Collection<String> names = new ArrayList<>(0);
        names.add(new Dataized(first.take("output")).asString());
        Phi entry = new FindNextFileFuncCall(win).make(handle);
        while (new Dataized(entry.take("code")).asNumber().intValue() == 0) {
            names.add(new Dataized(entry.take("output")).asString());
            entry = new FindNextFileFuncCall(win).make(handle);
        }
        new Dataized(
            new FindCloseFuncCall(win).make(handle).take("code")
        ).take();
        return names;
    }
}
