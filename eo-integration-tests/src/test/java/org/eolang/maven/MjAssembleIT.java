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
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for mojas.
 *
 * @since 0.52
 */
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleNotContainsTestWord",
    "PMD.UnitTestShouldIncludeAssert"
})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjAssembleIT {

    @Test
    void assemblesTogether(@Mktmp final Path temp) throws IOException {
        final String stdout = "target/eo/%s/org/eolang/io/stdout.%s";
        final String parsed = String.format(stdout, "1-parse", "xmir");
        final String pulled = String.format(stdout, "2-pull", "eo");
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files()
                    .file("src/main/eo/foo/x/main.eo")
                    .write(MjAssembleIT.program().getBytes(StandardCharsets.UTF_8));
                MjAssembleIT.registerAssemble(f);
                f.exec("package");
                MatcherAssert.assertThat(
                    String.format(
                        "AssembleMojo should have parsed stdout object %s, but didn't",
                        parsed
                    ),
                    f.files().file(parsed).exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    String.format(
                        "AssembleMojo should have pulled stdout object %s, but didn't",
                        pulled
                    ),
                    f.files().file(pulled).exists(),
                    Matchers.is(true)
                );
            }
        );
    }

    @Test
    void assemblesNotFailWithFailOnError(@Mktmp final Path temp) throws IOException {
        final String prog = String.join(
            "\n",
            "+alias stdout org.eolang.io.stdout",
            "+home https://github.com/objectionary/eo",
            "+package one",
            "+version 0.0.0\n",
            "# The seq *-1 leads to error.",
            "[x] > main",
            "  seq *-1 > @",
            "    true"
        );
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files()
                    .file("src/main/eo/one/main.eo")
                    .write(prog.getBytes(StandardCharsets.UTF_8));
                MjAssembleIT.registerAssemble(f);
                f.exec("test");
                MatcherAssert.assertThat(
                    "Even if the eo program invalid we still have to parse it, but we didn't",
                    temp.resolve("target/eo/1-parse/one/main.xmir").toAbsolutePath().toFile(),
                    FileMatchers.anExistingFile()
                );
            }
        );
    }

    private static String program() {
        return String.join(
            "\n",
            "+alias stdout org.eolang.io.stdout",
            "+package foo.y",
            "+version 0.1.1",
            "",
            "# Prints Hello World! to stdout.",
            "[x] > main",
            "  (stdout \"Hello World!\" x).print > @"
        );
    }

    private static Execution registerAssemble(final Farea farea) throws IOException {
        return new AppendedPlugin(farea).value()
            .goals("register", "assemble");
    }
}
