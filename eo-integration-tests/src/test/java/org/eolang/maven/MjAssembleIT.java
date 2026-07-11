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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for mojas.
 * @since 0.52
 * @todo #5078:60min Re-enable the assemble ITs ({@code assemblesTogether} and
 *  {@code assemblesNotFailWithFailOnError}) once the objectionary remote
 *  registry is re-published with the new parser's syntax. These tests run
 *  {@code mvn package}/{@code mvn test} against tiny EO programs that pull
 *  {@code tuple}, {@code seq}, {@code number}, etc. from the remote registry
 *  into {@code target/eo/2-pull/}. Those pulled {@code .eo} sources still
 *  contain pre-spec syntax (fluent {@code .method} continuation after
 *  horizontal-completed lines, the {@code ?} name-suffix modifier, etc.) that
 *  the new spec-strict parser rejects. The local {@code eo-runtime/src/main/eo/}
 *  files are already rewritten and pass; only the registry's published copies
 *  need a matching re-upload. To re-enable: publish the updated runtime to
 *  objectionary, then drop the {@link Disabled} annotations on both methods.
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjAssembleIT {

    @Test
    @Disabled("registry still serves pre-spec EO sources, see class javadoc")
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
    @Disabled("registry still serves pre-spec EO sources, see class javadoc")
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
            "# The seq *-1 leads to error.",
            "",
            "+alias stdout io.stdout",
            "+home https://github.com/objectionary/eo",
            "+package one",
            "+version 0.0.0",
            "",
            "[x] > main",
            "  seq *-1 > @",
            "    true"
        );
    }

    private static String program() {
        return String.join(
            System.lineSeparator(),
            "# Prints Hello World! to stdout.",
            "",
            "+alias stdout io.stdout",
            "+package foo.x",
            "+version 0.1.1",
            "",
            "[x] > main",
            "  (stdout \"Hello World!\" x).print > @"
        );
    }

    private static Execution registerAssemble(final Farea farea) throws IOException {
        return new AppendedPlugin(farea).value()
            .goals("register", "assemble");
    }
}
