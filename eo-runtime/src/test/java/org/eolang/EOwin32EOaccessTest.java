/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOwin32$EOaccess}.
 *
 * @since 0.77.0
 */
final class EOwin32EOaccessTest {

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void accessesFileWithNonAsciiName() throws IOException {
        final Path file = Files.createTempFile("Ж日本-", ".txt");
        MatcherAssert.assertThat(
            String.format("win32.access did not find the non-ASCII file %s", file),
            new Dataized(
                new PhApplication(
                    new EOwin32$EOaccess(),
                    new Bind("path", new Data.ToPhi(file.toString())),
                    new Bind("mode", new Data.ToPhi(0L))
                )
            ).asNumber().intValue(),
            Matchers.equalTo(0)
        );
    }
}
