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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for eo-maven-plugin:transpile goal.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjTranspileIT {

    @Test
    @SuppressWarnings("PMD.UnitTestShouldIncludeAssert")
    void transpilesWithPackage(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/one/foo.eo").write(
                    String.join(
                        "\n",
                        "+package one",
                        "",
                        "# no comments.",
                        "[] > foo",
                        "  QQ.io.stdout > @",
                        "    \"Hello, world!\\n\""
                    ).getBytes(StandardCharsets.UTF_8)
                );
                new AppendedPlugin(f).value()
                    .goals("register", "parse", "transpile");
                f.exec("process-sources");
                final String java = "EOfoo.java";
                final String pname = "EOone";
                final String pinfo = "package-info.java";
                MatcherAssert.assertThat(
                    String.format(
                        "The %s file must be generated, but it didn't",
                        java
                    ),
                    temp.resolve(
                        String.format(
                            "target/generated-sources/%s/%s",
                            pname,
                            java
                        )
                    ).toFile().exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    String.format(
                        "The %s file must contain the %s package name",
                        pinfo,
                        pname
                    ),
                    Files.readString(
                        temp.resolve(
                            String.format(
                                "target/generated-sources/%s/%s",
                                pname,
                                pinfo
                            )
                        ),
                        StandardCharsets.UTF_8
                    ),
                    Matchers.containsString(String.format("package %s;", pname))
                );
            }
        );
    }

    @Test
    @SuppressWarnings("PMD.UnitTestShouldIncludeAssert")
    void transpilesSimpleApp(@Mktmp final Path temp)
        throws Exception {
        final String prog = String.join(
            "\n",
            "# This is a random program in EO, which supposedly",
            "# complies with all syntactic rules of the language,",
            "# include the requirements for comments.",
            "[] > foo",
            "  QQ.io.stdout > @",
            "    \"Hello, world!\\n\"",
            ""
        );
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(prog.getBytes(StandardCharsets.UTF_8));
                new AppendedPlugin(f).value()
                    .goals("register", "parse", "transpile");
                f.exec("process-sources");
                final String java = "EOfoo.java";
                final String pinfo = "package-info.java";
                MatcherAssert.assertThat(
                    String.format(
                        "The %s file is re-generated",
                        java
                    ),
                    temp.resolve(
                        String.format(
                            "target/generated-sources/%s",
                            java
                        )
                    ).toFile().exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    String.format(
                        "The %s file must not exist, but it doesn't",
                        pinfo
                    ),
                    temp.resolve(
                        String.format(
                            "target/generated-sources/%s",
                            pinfo
                        )
                    ).toFile().exists(),
                    Matchers.is(false)
                );
            }
        );
    }
}
