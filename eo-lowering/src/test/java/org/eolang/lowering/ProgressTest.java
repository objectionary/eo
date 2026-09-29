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
 * Test case for {@link Progress}.
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
}
