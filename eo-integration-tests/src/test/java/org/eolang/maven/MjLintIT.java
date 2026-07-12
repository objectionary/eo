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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for eo-maven-plugin:lint goal.
 * @since 0.52
 * @todo #4394:35min Enable MjLints related tests after `lints` will be adjusted with `Φ` object.
 *  For now, lints checks for `Q` instead of `Φ`. After new version of lints released, we should
 *  enable these integration tests in MjLintIT, and others:
 *  {@link MjLintTest#doesNotFailWithNoErrorsAndWarnings},
 *  {@link MjLintTest#doesNotDetectWarningWithoutCorrespondingFlag},
 *  {@link MjLintTest#skipsAlreadyLinted}, {@link MjLintTest#savesVerifiedResultsToCache},
 *  {@link MjLintTest#getsAlreadyVerifiedResultsFromCache}
 *  {@link MjTranspileTest#recompilesIfModified}, {@link MjTranspileTest#recompilesIfExpired},
 *  {@link MjTranspileTest#doesNotRetranspileIfNotModified},
 *  {@link MjTranspileTest#transpilesSimpleEoProgram},
 *  {@link MjTranspileTest#transpilesSeveralEoProgramsInParallel},
 *  {@link MjTranspileTest#transpilesSourcesForDifferentScopesWithoutIntersections}.
 */
@Disabled
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjLintIT {

    @Test
    void lintsAgainAfterModification(@Mktmp final Path temp) throws Exception {
        final String source = "src/main/eo/foo.eo";
        final String xmir = "target/eo/3-lint/foo.xmir";
        final byte[] prog = MjLintIT.helloWorld().getBytes(StandardCharsets.UTF_8);
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
                    .file("src/main/eo/foo.eo")
                    .write(MjLintIT.helloWorld().getBytes(StandardCharsets.UTF_8));
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

    private static String helloWorld() {
        return String.join(
            System.lineSeparator(),
            "+alias stdout io.stdout",
            "+home https://www.eolang.org",
            "+package foo.x",
            "+version 0.0.0",
            "",
            "[x] > main",
            "  (stdout \"Hello!\" x).print > @"
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
