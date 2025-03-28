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
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for mojas.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjAssembleIT {

    @Test
    void assemblesTogether(@Mktmp final Path temp) throws IOException {
        final String stdout = "target/eo/%s/org/eolang/io/stdout.%s";
        final String parsed = String.format(stdout, MjParse.DIR, MjAssemble.XMIR);
        final String optimized = String.format(stdout, MjShake.DIR, MjAssemble.XMIR);
        final String pulled = String.format(stdout, MjPull.DIR, MjAssemble.EO);
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files()
                    .file("src/main/eo/one/main.eo")
                    .write(MjAssembleIT.helloWorld().getBytes(StandardCharsets.UTF_8));
                MjAssembleIT.appendItself(f);
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
                        "AssembleMojo should have optimized stdout object %s, but didn't",
                        optimized
                    ),
                    f.files().file(optimized).exists(),
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
            "+package test",
            "+version 0.0.0",
            "",
            "[x] < wrong>",
            "  (stdout \"Hello!\" x).print"
        );
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files()
                    .file("src/main/eo/one/main.eo")
                    .write(prog.getBytes(StandardCharsets.UTF_8));
                MjAssembleIT.appendItself(f);
                f.exec("test");
                MatcherAssert.assertThat(
                    "Even if the eo program invalid we still have to parse it, but we didn't",
                    temp.resolve(String.format("target/eo/%s", MjParse.DIR)).toAbsolutePath(),
                    new ContainsFiles(String.format("**/main.%s", MjAssemble.XMIR))
                );
                MatcherAssert.assertThat(
                    "Even if the eo program invalid we still have to optimize it, but we didn't",
                    temp.resolve(String.format("target/eo/%s", MjShake.DIR)).toAbsolutePath(),
                    new ContainsFiles(String.format("**/main.%s", MjAssemble.XMIR))
                );
            }
        );
    }

    @Test
    void configuresChildParameters(@Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files()
                    .file("src/main/eo/one/main.eo")
                    .write(MjAssembleIT.helloWorld().getBytes(StandardCharsets.UTF_8));
                MjAssembleIT.appendItself(f)
                    .configuration()
                    .set("trackTransformationSteps", Boolean.TRUE.toString());
                f.exec("test");
                MatcherAssert.assertThat(
                    String.join(
                        "\n",
                        "AssembleMojo should have configured parameters",
                        "within the Mojos that it uses, but it didn't"
                    ),
                    f.files().file(
                        String.format(
                            "target/eo/%s/one/main.%s",
                            MjShake.DIR,
                            MjAssemble.XMIR
                        )
                    ).exists(),
                    Matchers.is(true)
                );
            }
        );
    }

    private static String helloWorld() {
        return String.join(
            "\n",
            "+alias stdout org.eolang.io.stdout",
            "+home https://www.eolang.org",
            "+package foo.x",
            "+version 0.0.0",
            "",
            "# No comments.",
            "[x] > main",
            "  (stdout \"Hello!\" x).print > @"
        );
    }

    private static Execution appendItself(final Farea farea) throws IOException {
        return new AppendedPlugin(farea).value()
            .goals("register", "assemble");
    }
}
