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
 * Test case for {@link EOposix$EOlstat}.
 *
 * @since 0.77.0
 */
final class EOposixEOlstatTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void seesASymbolicLinkAsItself(@TempDir final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "lstat must not report the directory behind the link instead of the link itself",
            new Dataized(
                new PhApplication(
                    new EOposix$EOlstat(),
                    new Bind(
                        "path",
                        new Data.ToPhi(
                            Files.createSymbolicLink(
                                temp.resolve("ярлык"),
                                Files.createDirectory(temp.resolve("борщ"))
                            ).toString()
                        )
                    )
                ).take("mode")
            ).asNumber().longValue() / 4096L,
            Matchers.equalTo(10L)
        );
    }
}
