/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Progress}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class ProgressTest {

    @Test
    void countsTheEntriesMorphedSoFar(@Mktmp final Path temp) throws IOException {
        final Progress progress = new Progress(7);
        progress.add(Files.write(temp.resolve("3.xml"), new byte[13]));
        progress.add(Files.write(temp.resolve("5.xml"), new byte[29]));
        MatcherAssert.assertThat(
            "the status must count the entries morphed so far, but it doesnt",
            progress.asString(),
            Matchers.startsWith("2 of 7 entries")
        );
    }

    @Test
    void sumsTheBytesOfTheProtocolsWrittenSoFar(@Mktmp final Path temp) throws IOException {
        final Progress progress = new Progress(4);
        progress.add(Files.write(temp.resolve("1.xml"), new byte[211]));
        progress.add(Files.write(temp.resolve("2.xml"), new byte[437]));
        MatcherAssert.assertThat(
            "the status must sum the bytes of the protocols written so far, but it doesnt",
            progress.asString(),
            Matchers.containsString("648b of protocols")
        );
    }

    @Test
    void countsTheProtocolsTakenFromTheCache(@Mktmp final Path temp) throws IOException {
        final Progress progress = new Progress(9);
        progress.add(Files.write(temp.resolve("4.xml"), new byte[17]));
        progress.reuse(Files.write(temp.resolve("6.xml"), new byte[23]));
        progress.reuse(Files.write(temp.resolve("8.xml"), new byte[31]));
        MatcherAssert.assertThat(
            "the status must count the protocols taken from the cache, but it doesnt",
            progress.asString(),
            Matchers.containsString("2 of them from cache")
        );
    }

    @Test
    void countsTheReusedProtocolsAmongTheMorphedEntries(@Mktmp final Path temp)
        throws IOException {
        final Progress progress = new Progress(5);
        progress.add(Files.write(temp.resolve("2.xml"), new byte[41]));
        progress.reuse(Files.write(temp.resolve("3.xml"), new byte[19]));
        MatcherAssert.assertThat(
            "the status must count a reused protocol as a morphed entry, but it doesnt",
            progress.asString(),
            Matchers.startsWith("2 of 5 entries")
        );
    }
}
