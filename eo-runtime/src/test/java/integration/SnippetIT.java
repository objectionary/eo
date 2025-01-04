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
package integration;

import com.jcabi.manifests.Manifests;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;
import org.cactoos.iterable.Mapped;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Integration test for simple snippets.
 *
 * <p>This test will/may fail if you change something in {@code to-java.xsl}
 * or some other place where Java sources are generated. This happens
 * because this test relies on {@code eo-runtime.jar}, which it finds in Maven
 * Central. Thus, when changes are made, it is recommended to disable this test.
 * Then, when new {@code eo-runtime.jar} is
 * released to Maven Central, you enable this test again.</p>
 *
 * @since 0.1
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith(MktmpResolver.class)
final class SnippetIT {
    @ParameterizedTest
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    @ClasspathSource(value = "org/eolang/snippets/", glob = "**.yaml")
    @SuppressWarnings("unchecked")
    void runsAllSnippets(final String yml, final @Mktmp Path temp) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(yml));
        final String file = xtory.map().get("file").toString();
        Assumptions.assumeFalse(xtory.map().containsKey("skip"));
        new Farea(temp).together(
            f -> {
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.files().file("src/main").save(
                    Paths.get(System.getProperty("user.dir")).resolve("src/main")
                );
                f.files()
                    .file(String.format("src/main/eo/%s", file))
                    .write(
                        String.format(
                            "%s\n",
                            xtory.map().get("eo")
                        ).getBytes(StandardCharsets.UTF_8)
                    );
                f.dependencies().appendItself();
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty(
                            "eo.version",
                            Manifests.read("EO-Version")
                        )
                    )
                    .execution("compile")
                    .phase("generate-sources")
                    .goals("register", "assemble", "lint", "transpile")
                    .configuration()
                    .set("failOnWarning", Boolean.FALSE.toString());
                f.build()
                    .plugins()
                    .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
                    .execution("run")
                    .phase("test")
                    .goals("java")
                    .configuration()
                    .set("mainClass", "org.eolang.Main")
                    .set("arguments", xtory.map().get("args"));
                f.exec("clean", "test");
                MatcherAssert.assertThat(
                    String.format("'%s' printed something wrong", yml),
                    f.log().content(),
                    Matchers.allOf(
                        new Mapped<>(
                            ptn -> Matchers.matchesPattern(
                                Pattern.compile(ptn, Pattern.DOTALL | Pattern.MULTILINE)
                            ),
                            (Iterable<String>) xtory.map().get("out")
                        )
                    )
                );
            }
        );
    }
}
