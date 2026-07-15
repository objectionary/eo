/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import com.yegor256.farea.RequisiteMatcher;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for eo-maven-plugin:transpile goal.
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjTranspileIT {

    @Test
    void transpilesWithPackage(@Mktmp final Path temp) throws Exception {
        final String java = "EOfoo.java";
        final String pname = "EOone";
        final String pinfo = "package-info.java";
        new Farea(temp).together(
            f -> {
                MjTranspileIT.transpile(f, "src/main/eo/one/foo.eo", MjTranspileIT.withPackage());
                MatcherAssert.assertThat(
                    String.format("The %s file must be generated, but it didn't", java),
                    temp.resolve(MjTranspileIT.path(pname, java)).toFile().exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    String.format("The %s file must contain the %s package name", pinfo, pname),
                    Files.readString(
                        temp.resolve(MjTranspileIT.path(pname, pinfo)),
                        StandardCharsets.UTF_8
                    ),
                    Matchers.containsString(String.format("package org.eolang.%s;", pname))
                );
            }
        );
    }

    @Test
    void transpilesSimpleApp(@Mktmp final Path temp) throws Exception {
        final String java = "EOfoo.java";
        final String pinfo = "package-info.java";
        new Farea(temp).together(
            f -> {
                MjTranspileIT.transpile(f, "src/main/eo/foo.eo", MjTranspileIT.simpleApp());
                MatcherAssert.assertThat(
                    String.format("The %s file is re-generated", java),
                    temp.resolve(String.format("target/generated-sources/org/eolang/%s", java))
                        .toFile().exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    String.format("The %s file must exist, but it doesn't", pinfo),
                    temp.resolve(String.format("target/generated-sources/org/eolang/%s", pinfo))
                        .toFile().exists(),
                    Matchers.is(true)
                );
            }
        );
    }

    private static void transpile(
        final Farea farea, final String path, final String source
    ) throws java.io.IOException {
        farea.clean();
        farea.files().file(path).write(source.getBytes(StandardCharsets.UTF_8));
        new AppendedPlugin(farea).value()
            .goals("register", "parse", "transpile");
        farea.exec("process-sources");
        MatcherAssert.assertThat(
            "the build must succeed, but it didn't",
            farea.log(),
            RequisiteMatcher.SUCCESS
        );
    }

    private static String path(final String pname, final String file) {
        return String.format("target/generated-sources/org/eolang/%s/%s", pname, file);
    }

    private static String withPackage() {
        return String.join(
            System.lineSeparator(),
            "+package one",
            "",
            "[] > foo",
            "  Q.io.stdout > @",
            "    \"Hello, world!\\n\""
        );
    }

    private static String simpleApp() {
        return String.join(
            System.lineSeparator(),
            "[] > foo",
            "  Q.io.stdout > @",
            "    \"Hello, world!\\n\"",
            ""
        );
    }
}
