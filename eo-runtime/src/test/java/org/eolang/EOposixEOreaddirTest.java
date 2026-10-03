/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import org.eolang.posix.CStdLib;
import org.eolang.sys.Handles;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
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
            EOposixEOreaddirTest.walked(temp),
            Matchers.containsInAnyOrder(".", "..", "плюшка", "щи")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsAFailedReadApartFromTheEnd(@TempDir final Path temp) {
        final Phi handle = new Data.ToPhi(
            new Dataized(
                new PhApplication(
                    new EOposix$EOopendir(), "path", new Data.ToPhi(temp.toString())
                ).take("code")
            ).asNumber().intValue()
        );
        CStdLib.INSTANCE.close(
            Native.load("c", EOposixEOreaddirTest.Dirs.class).dirfd(
                Handles.INSTANCE.get(
                    "the test stream", new Dataized(handle).asNumber().intValue()
                )
            )
        );
        final Phi entry = EOposixEOreaddirTest.entry(handle);
        MatcherAssert.assertThat(
            "a read from a stream whose descriptor is closed must fail with code -2, not end with -1 (see #9050)",
            new Dataized(entry.take("code")).asNumber().intValue(),
            Matchers.equalTo(-2)
        );
    }

    private static Collection<String> walked(final Path path) {
        final Phi handle = new Data.ToPhi(
            new Dataized(
                new PhApplication(
                    new EOposix$EOopendir(), "path", new Data.ToPhi(path.toString())
                ).take("code")
            ).asNumber().intValue()
        );
        final Collection<String> names = new ArrayList<>(0);
        Phi entry = EOposixEOreaddirTest.entry(handle);
        while (new Dataized(entry.take("code")).asNumber().intValue() == 0) {
            names.add(new Dataized(entry.take("name")).asString());
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

    /**
     * The part of libc that hands out the descriptor of a directory stream.
     *
     * @since 0.77.0
     */
    private interface Dirs extends Library {
        /**
         * The descriptor of a directory stream.
         *
         * @param dirp The stream
         * @return The descriptor
         */
        int dirfd(Pointer dirp);
    }
}
