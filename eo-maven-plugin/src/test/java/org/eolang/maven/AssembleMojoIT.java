/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
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
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
final class AssembleMojoIT {

    @Test
    void assemblesTogether(@Mktmp final Path temp) throws IOException {
        final String stdout = "target/eo/%s/org/eolang/io/stdout.%s";
        final String parsed = String.format(stdout, ParseMojo.DIR, AssembleMojo.XMIR);
        final String optimized = String.format(stdout, ShakeMojo.DIR, AssembleMojo.XMIR);
        final String pulled = String.format(stdout, PullMojo.DIR, AssembleMojo.EO);
        new Farea(temp).together(
            f -> {
                f.clean();
                f.dependencies()
                    .append("org.eolang", "eo-runtime", "0.51.6");
                f.files()
                    .file("src/main/eo/one/main.eo")
                    .write(AssembleMojoIT.helloWorld().getBytes(StandardCharsets.UTF_8));
                f.build()
                    .plugins()
                    .appendItself()
                    .execution("tests")
                    .goals("register", "assemble");
                f.exec("package");
                MatcherAssert.assertThat(
                    String.join(
                        " ",
                        "AssembleMojo should have placed runtime",
                        "library, but didn't"
                    ),
                    f.files().file(
                        String.format(
                            "target/eo/%s/org.eolang/eo-runtime/-/0.51.6/org/eolang/Phi.class",
                            ResolveMojo.DIR
                        )
                    ).exists(),
                    Matchers.is(true)
                );
            }
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have parsed stdout object %s, but didn't",
                parsed
            ),
            temp.resolve(parsed).toFile().exists(),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have optimized stdout object %s, but didn't",
                optimized
            ),
            temp.resolve(optimized).toFile().exists(),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have pulled stdout object %s, but didn't",
                pulled
            ),
            temp.resolve(pulled).toFile().exists(),
            Matchers.is(true)
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
                f.build()
                    .plugins()
                    .appendItself()
                    .execution("tests")
                    .goals("register", "parse", "shake");
                f.exec("test");
            }
        );
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to parse it, but we didn't",
            temp.resolve(String.format("target/eo/%s", ParseMojo.DIR)).toAbsolutePath(),
            new ContainsFiles(String.format("**/main.%s", AssembleMojo.XMIR))
        );
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to optimize it, but we didn't",
            temp.resolve(String.format("target/eo/%s", ShakeMojo.DIR)).toAbsolutePath(),
            new ContainsFiles(String.format("**/main.%s", AssembleMojo.XMIR))
        );
    }

    @Test
    void configuresChildParameters(@Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files()
                    .file("src/main/eo/one/main.eo")
                    .write(AssembleMojoIT.helloWorld().getBytes(StandardCharsets.UTF_8));
                f.build()
                    .plugins()
                    .appendItself()
                    .execution("tests")
                    .goals("register", "assemble")
                    .configuration()
                    .set("trackTransformationSteps", Boolean.TRUE.toString());
                f.exec("test");
            }
        );
        MatcherAssert.assertThat(
            "AssembleMojo should have configured parameters within the Mojos that it uses, but it didn't",
            temp.resolve(
                String.format("target/eo/%s/one/main.%s", ShakeMojo.DIR, AssembleMojo.XMIR)
            ).toFile().exists(),
            Matchers.is(true)
        );
    }

    private static String helloWorld() {
        return String.join(
            "\n",
            "+alias stdout org.eolang.io.stdout",
            "+home https://www.eolang.org",
            "+package foo.x",
            "+unlint object-has-data",
            "+unlint broken-alias-second",
            "+unlint incorrect-alias",
            "+version 0.0.0",
            "",
            "# No comments.",
            "[x] > main",
            "  (stdout \"Hello!\" x).print > @"
        );
    }
}
