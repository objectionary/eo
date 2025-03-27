/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for {@link MjLint}.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class, RandomProgramResolver.class})
final class MjLintIT {

    @Test
    void lintsAgainAfterModification(@Mktmp final Path temp, @RandomProgram final String program)
        throws Exception {
        final String source = "src/main/eo/foo.eo";
        final String xmir = String.format("target/eo/%s/foo.xmir", MjLint.DIR);
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file(source).write(program.getBytes());
                MjLintIT.appendItself(f)
                    .configuration()
                    .set("failOnWarning", "false");
                f.exec("process-classes");
                final long before = f.files()
                    .file(xmir)
                    .path()
                    .toFile()
                    .lastModified();
                f.files().file(source).write(program.getBytes());
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "the .xmir file is re-generated",
                    f.files().file(xmir).path().toFile().lastModified(),
                    Matchers.not(Matchers.equalTo(before))
                );
            }
        );
    }

    @Test
    void printsLintsUrlWithVersion(@Mktmp final Path temp, @RandomProgram final String program)
        throws IOException {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(program.getBytes());
                MjLintIT.appendItself(f)
                    .configuration()
                    .set("failOnWarning", "false");
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "Lints URL was not printed, but it should",
                    f.log().content(),
                    Matchers.matchesPattern(
                        String.join(
                            " ",
                            "(?s).*\\[INFO] Read more about lints:",
                            "https://www\\.objectionary\\.com/lints/\\d+\\.\\d+\\.\\d+.*"
                        )
                    )
                );
            }
        );
    }

    private static Execution appendItself(final Farea farea) throws IOException {
        return new AppendedPlugin(farea).value()
            .goals("register", "parse", "shake", "lint");
    }
}
