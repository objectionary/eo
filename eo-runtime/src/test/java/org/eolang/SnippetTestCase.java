/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang;

import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.cactoos.iterable.Mapped;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.jucs.ClasspathSource;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.yaml.snakeyaml.Yaml;

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
 *
 * @todo #2718:30min One snippet is disabled now, in
 *  the "src/test/resources/snippets/*.yaml" because there's no
 *  "sprintf" object in objectionary (fibo.yaml).
 *  When "sprintf" is in objectionary again - we need to enable
 *  it (by removing the "skip" attribute from the YAML file).
 */
@ExtendWith(WeAreOnline.class)
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class SnippetTestCase {

    /**
     * Temp dir.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @TempDir
    public Path temp;

    /**
     * Integration test.
     * @param yml The YAML
     * @throws IOException If fails
     */
    @ParameterizedTest
    @ExtendWith(WeAreOnline.class)
    @SuppressWarnings("unchecked")
    @ClasspathSource(value = "org/eolang/snippets/", glob = "**.yaml")
    void runsAllSnippets(final String yml) throws IOException {
        final Yaml yaml = new Yaml();
        final Map<String, Object> map = yaml.load(yml);
        final String file = map.get("file").toString();
        Assumptions.assumeFalse(map.containsKey("skip"));
        new Farea(this.temp).together(
            f -> {
                f.properties()
                    .set("project.build.sourceEncoding", "UTF-8")
                    .set("project.reporting.outputEncoding", "UTF-8");
                f.files()
                    .file(String.format("src/main/eo/%s", file))
                    .write(String.format("%s\n", map.get("eo")))
                    .show();
                final Path runtime = Paths.get(System.getProperty("user.dir"))
                    .resolve("src/main/eo");
                final Collection<Path> sources = Files.walk(runtime)
                    .filter(src -> !src.toFile().isDirectory())
                    .collect(Collectors.toList());
                for (final Path src : sources) {
                    f.files()
                        .file(String.format("src/main/eo/%s", runtime.relativize(src)))
                        .write(new UncheckedText(new TextOf(src)).asString())
                        .show();
                }
                f.dependencies()
                    .appendItself();
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty("eo.version", "1.0-SNAPSHOT")
                    )
                    .phase("generate-sources")
                    .goals("register", "assemble", "verify", "transpile")
                    .configuration()
                    .set("failOnWarnings", "true");
                f.build()
                    .plugins()
                    .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
                    .phase("test")
                    .goals("java")
                    .configuration()
                    .set("mainClass", "org.eolang.Main")
                    .set("arguments", map.get("args"));
                f.exec("clean", "test");
                MatcherAssert.assertThat(
                    String.format("'%s' printed something wrong", yml),
                    f.log(),
                    Matchers.allOf(
                        new Mapped<>(
                            ptn -> Matchers.matchesPattern(
                                Pattern.compile(ptn, Pattern.DOTALL | Pattern.MULTILINE)
                            ),
                            (Iterable<String>) map.get("out")
                        )
                    )
                );
            }
        );
    }
}
