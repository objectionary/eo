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

import com.jcabi.log.Logger;
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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
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
 * @since 0.1
 * @todo #2718:30min One snippets is disabled now, in
 *  the "src/test/resources/snippets/*.yaml". It needs
 *  "sprintf" object in objectionary (fibo.yaml).
 *  When "sprintf" is in objectionary again - we need to enable
 *  it (by removing the "skip" attribute from the YAML file).
 */
@ExtendWith(WeAreOnline.class)
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
final class SnippetCaseTest {
    @ParameterizedTest
    @Tag("slow")
    @ExtendWith(WeAreOnline.class)
    @ClasspathSource(value = "org/eolang/snippets/", glob = "**.yaml")
    void runsAllSnippets(final String yml, final @TempDir Path temp) throws IOException {
        final Yaml yaml = new Yaml();
        final Map<String, Object> map = yaml.load(yml);
        final String file = map.get("file").toString();
        Assumptions.assumeFalse(map.containsKey("skip"));
        new Farea(temp).together(
            f -> {
                f.properties()
                    .set("project.build.sourceEncoding", "UTF-8")
                    .set("project.reporting.outputEncoding", "UTF-8");
                f.files()
                    .file(String.format("src/main/eo/%s", file))
                    .write(String.format("%s\n", map.get("eo")))
                    .show();
                SnippetCaseTest.copySources(f, "src/main/eo");
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
                f.dependencies().appendItself();
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty("eo.version", "1.0-SNAPSHOT")
                    )
                    .execution("compile")
                    .phase("generate-sources")
                    .goals("register", "assemble", "verify", "transpile")
                    .configuration()
                    .set("failOnWarnings", "true");
                f.build()
                    .plugins()
                    .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
                    .execution("run")
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

    @Test
    @Tag("slow")
    @ExtendWith(WeAreOnline.class)
    void runsTestsAfterPhiAndUnphi(final @TempDir Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                SnippetCaseTest.copySources(f, "src/main");
                SnippetCaseTest.copySources(f, "src/test/eo");
                f.properties()
                    .set("project.build.sourceEncoding", "UTF-8")
                    .set("project.reporting.outputEncoding", "UTF-8");
                f.dependencies().append(
                    "net.sf.saxon",
                    "Saxon-HE",
                    "12.4"
                );
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty("eo.version", "1.0-SNAPSHOT")
                    )
                    .execution("phi-unphi")
                    .phase("compile")
                    .goals(
                        "register",
                        "parse",
                        "optimize",
                        "xmir-to-phi",
                        "phi-to-xmir",
                        "print"
                    )
                    .configuration()
                    .set("sourcesDir", "${project.basedir}/src/test/eo")
                    .set("targetDir", "${project.build.directory}/eo-test")
                    .set("phiInputDir", "${project.build.directory}/eo-test/2-optimize")
                    .set("phiOutputDir", "${project.build.directory}/phi")
                    .set("unphiInputDir", "${project.build.directory}/phi")
                    .set("unphiOutputDir", "${project.build.directory}/generated-eo-test/1-parse")
                    .set("unphiMetas", new String[]{"+tests"})
                    .set("printSourcesDir", "${project.build.directory}/generated-eo-test/1-parse")
                    .set("printOutputDir", "${project.basedir}/src/test/generated-eo")
                    .set("printReversed", "true");
                f.exec("clean", "compile");
                final String phi = f.log();
                MatcherAssert.assertThat(
                    "Converting to phi and back was not successful",
                    phi,
                    Matchers.containsString("BUILD SUCCESS")
                );
                f.files().file("pom.xml").delete();
                f.properties()
                    .set("project.build.sourceEncoding", "UTF-8")
                    .set("project.reporting.outputEncoding", "UTF-8");
                f.dependencies().append(
                    "org.junit.jupiter",
                    "junit-jupiter-api",
                    "5.10.2"
                );
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty("eo.version", "1.0-SNAPSHOT")
                    )
                    .execution("compile")
                    .goals(
                        "register",
                        "assemble",
                        "verify",
                        "transpile",
                        "copy",
                        "unplace",
                        "unspile"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.csv")
                    .set("foreignFormat", "csv")
                    .set("failOnWarning", "false")
                    .set("offline", "true")
                    .set("withRuntimeDependency", "false")
                    .set("placeBinariesThatHaveSources", "true");
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty("eo.version", "1.0-SNAPSHOT")
                    )
                    .execution("tests")
                    .phase("generate-test-sources")
                    .goals(
                        "register",
                        "assemble",
                        "verify",
                        "transpile",
                        "binarize"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.csv")
                    .set("foreignFormat", "csv")
                    .set("failOnWarning", "false")
                    .set("offline", "true")
                    .set("scope", "test")
                    .set("sourcesDir", "${project.basedir}/src/test/generated-eo")
                    .set("targetDir", "${project.basedir}/target/eo-test")
                    .set("addSourcesRoot", "false")
                    .set("addTestSourcesRoot", "true")
                    .set("failOnWarning", "false")
                    .set("generatedDir", "${project.basedir}/target/generated-test-sources")
                    .set("withRuntimeDependency", "false")
                    .set("placeBinariesThatHaveSources", "true");
                f.exec("clean", "test");
                final String log = f.log();
                Logger.debug(this, log);
                MatcherAssert.assertThat(
                    "Some tests weren't passed after converting to phi and back",
                    log,
                    Matchers.containsString("BUILD SUCCESS")
                );
            }
        );
    }

    /**
     * Copy EO sources.
     * @param farea Farea instance
     * @param target Directory to copy from
     * @throws IOException If fails to copy files
     * @todo #3092:30min Remove filter with "rust-tests.eo". BinarizeMojo does not add rust
     *  dependencies to rust-tests after phi->unphi (see:
     *  <a href="https://github.com/objectionary/eo/issues/3145">this</a> for details).
     *  When it's resolved we need to remove the filter and make sure the snippet test
     *  {@link SnippetCaseTest#runTestsAfterPhiAndUnphi} still works.
     */
    private static void copySources(final Farea farea, final String target) throws IOException {
        final Path runtime = Paths.get(System.getProperty("user.dir"))
            .resolve(target);
        final Collection<Path> sources = Files.walk(runtime)
            .filter(src -> !src.toFile().isDirectory())
            .filter(src -> !src.endsWith("rust-tests.eo"))
            .collect(Collectors.toList());
        for (final Path src : sources) {
            farea.files()
                .file(String.format("%s/%s", target, runtime.relativize(src)))
                .write(new UncheckedText(new TextOf(src)).asString())
                .show();
        }
    }
}
