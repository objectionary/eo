/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.SecureRandom;
import org.eolang.Dataized;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link Stat}.
 *
 * @since 0.77.0
 */
final class StatTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void answersWithTheSizeRightOnTheReturn(@TempDir final Path temp) throws IOException {
        final byte[] content = new byte[new SecureRandom().nextInt(4096) + 17];
        MatcherAssert.assertThat(
            "the size must sit on the answer itself, not on an object handed back beside the code",
            new Dataized(
                new Stat(
                    Files.write(temp.resolve("весы.bin"), content).toString(),
                    (path, buf) -> CStdLib.INSTANCE.stat(path, buf)
                ).it().take("size")
            ).asNumber().longValue(),
            Matchers.equalTo((long) content.length)
        );
    }
}
