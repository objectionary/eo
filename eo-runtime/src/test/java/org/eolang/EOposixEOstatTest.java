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
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOposix$EOstat}.
 *
 * @since 0.77.0
 */
final class EOposixEOstatTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void seesADirectoryBehindASymbolicLink(@TempDir final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "stat must not report the link itself instead of the directory it points at",
            new Dataized(
                new PhApplication(
                    new EOposix$EOstat(),
                    new Bind(
                        "path",
                        new Data.ToPhi(
                            Files.createSymbolicLink(
                                temp.resolve("ссылка"),
                                Files.createDirectory(temp.resolve("щи"))
                            ).toString()
                        )
                    )
                ).take("mode")
            ).asNumber().longValue() / 4096L,
            Matchers.equalTo(4L)
        );
    }
}
