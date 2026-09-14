/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Structure;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.ToIntBiFunction;
import org.eolang.posix.CStdLib;
import org.eolang.posix.StatSyscall;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link StatSyscall}.
 *
 * @since 0.74.0
 */
final class StatSyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void seesASymbolicLinkAsItselfWithLstat(@TempDir final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "lstat reports the directory behind the link instead of the link itself",
            this.type(
                Files.createSymbolicLink(
                    temp.resolve("ссылка"), Files.createDirectory(temp.resolve("щи"))
                ),
                (path, buf) -> CStdLib.INSTANCE.lstat(path, buf)
            ),
            Matchers.equalTo(10L)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void seesADirectoryBehindASymbolicLinkWithStat(@TempDir final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "stat reports the link itself instead of the directory it points at",
            this.type(
                Files.createSymbolicLink(
                    temp.resolve("ссылка"), Files.createDirectory(temp.resolve("щи"))
                ),
                (path, buf) -> CStdLib.INSTANCE.stat(path, buf)
            ),
            Matchers.equalTo(4L)
        );
    }

    private long type(final Path path, final ToIntBiFunction<String, Structure> call) {
        return new Dataized(
            new StatSyscall(Phi.Φ.take("posix").copy(), call)
                .make(new Data.ToPhi(path.toString()))
                .take("output")
                .take("mode")
        ).asNumber().longValue() / 4096L;
    }
}
