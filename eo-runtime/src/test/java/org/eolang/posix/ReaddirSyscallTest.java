/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

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
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ReaddirSyscall}.
 * @since 0.76.0
 */
final class ReaddirSyscallTest {

    @Test
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "a number naming no open stream must be refused before it reaches libc as an address",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new ReaddirSyscall(Phi.Φ.take("posix").copy()).make(
                    new Data.ToPhi(-42)
                ),
                "reading a stream that was never opened was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("'dirp' argument of readdir")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void readsEveryNameTheDirectoryHolds(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("плюшка"), new byte[0]);
        Files.createDirectory(temp.resolve("щи"));
        MatcherAssert.assertThat(
            "the stream must report both children and the two dots, and nothing else",
            ReaddirSyscallTest.walked(temp),
            Matchers.containsInAnyOrder(".", "..", "плюшка", "щи")
        );
    }

    private static Collection<String> walked(final Path path) {
        final Phi posix = Phi.Φ.take("posix").copy();
        final Phi handle = new Data.ToPhi(
            new Dataized(
                new OpendirSyscall(posix)
                    .make(new Data.ToPhi(path.toString()))
                    .take("code")
            ).asNumber().intValue()
        );
        final Collection<String> names = new ArrayList<>(0);
        Phi entry = new ReaddirSyscall(posix).make(handle);
        while (new Dataized(entry.take("code")).asNumber().intValue() == 0) {
            names.add(new Dataized(entry.take("output")).asString());
            entry = new ReaddirSyscall(posix).make(handle);
        }
        new Dataized(
            new ClosedirSyscall(posix).make(handle).take("code")
        ).take();
        return names;
    }
}
