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
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjAssembleIT {

    @Test
    void assemblesTogether(@Mktmp final Path temp) throws IOException {
        final String stdout = "target/eo/%s/io/stdout.%s";
        final String parsed = String.format(stdout, "1-parse", "xmir");
        final String pulled = String.format(stdout, "2-pull", "eo");
        new Farea(temp).together(
            f -> {
                MjAssembleIT.prepare(f, "src/main/eo/foo/x/main.eo", MjAssembleIT.program());
                f.exec("package");
                MatcherAssert.assertThat(
                    String.format("AssembleMojo should have parsed stdout %s, but didn't", parsed),
                    f.files().file(parsed).exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    String.format("AssembleMojo should have pulled stdout %s, but didn't", pulled),
                    f.files().file(pulled).exists(),
                    Matchers.is(true)
                );
            }
        );
    }

    @Test
    void assemblesNotFailWithFailOnError(@Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                MjAssembleIT.prepare(f, "src/main/eo/one/main.eo", MjAssembleIT.failing());
                f.exec("test");
                MatcherAssert.assertThat(
                    "Even if the eo program invalid we still have to parse it, but we didn't",
                    temp.resolve("target/eo/1-parse/one/main.xmir").toAbsolutePath().toFile(),
                    FileMatchers.anExistingFile()
                );
            }
        );
    }

    private static void prepare(
        final Farea farea, final String path, final String source
    ) throws IOException {
        farea.clean();
        farea.files().file(path).write(source.getBytes(StandardCharsets.UTF_8));
        MjAssembleIT.registerAssemble(farea);
    }

    private static String failing() {
        return String.join(
            System.lineSeparator(),
            "+alias stdout io.stdout",
            "+home https://github.com/objectionary/eo",
            "+package one",
            "+version 0.0.0",
            "",
            "# The seq *-1 leads to error.",
            "[x] > main",
            "  seq *-1 > @",
            "    true"
        );
    }

    private static String program() {
        return String.join(
            System.lineSeparator(),
            "+alias stdout io.stdout",
            "+package foo.x",
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
