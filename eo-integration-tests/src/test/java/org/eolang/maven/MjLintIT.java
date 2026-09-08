/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Execution;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for eo-maven-plugin:lint goal.
 *
 * @since 0.52
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjLintIT {

    @Test
    void lintsAgainAfterModification(@Mktmp final Path temp) throws Exception {
        final String source = "src/main/eo/foo/x/main.eo";
        final String xmir = "target/eo/3-lint/foo/x/main.xmir";
        final byte[] prog = MjLintIT.program().getBytes(StandardCharsets.UTF_8);
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file(source).write(prog);
                MjLintIT.appendItself(f)
                    .configuration()
                    .set("failOnWarning", "false");
                f.exec("process-classes");
                final long before = f.files()
                    .file(xmir)
                    .path()
                    .toFile()
                    .lastModified();
                f.files().file(source).write(prog);
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    String.format("the .xmir file is re-generated past %d", before),
                    f.files().file(xmir).path().toFile().lastModified(),
                    Matchers.not(Matchers.equalTo(before))
                );
            }
        );
    }

    @Test
    void printsLintsUrlWithVersion(@Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files()
                    .file("src/main/eo/foo/x/main.eo")
                    .write(MjLintIT.program().getBytes(StandardCharsets.UTF_8));
                MjLintIT.appendItself(f)
                    .configuration()
                    .set("failOnWarning", "false");
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "Lints URL was not printed, but it should",
                    f.log().content(),
                    Matchers.matchesPattern(MjLintIT.lintsUrl())
                );
            }
        );
    }

    private static String program() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+home https://www.eolang.org",
            "+package foo.x",
            "+version 0.0.0",
            "",
            "[x] > main",
            "  x > @"
        );
    }

    private static String lintsUrl() {
        return String.join(
            " ",
            "(?s).*\\[INFO] Read more about lints:",
            "https://www\\.objectionary\\.com/lints/\\d+\\.\\d+\\.\\d+.*"
        );
    }

    private static Execution appendItself(final Farea farea) throws IOException {
        return new AppendedPlugin(farea).value()
            .goals("register", "parse", "lint");
    }
}
