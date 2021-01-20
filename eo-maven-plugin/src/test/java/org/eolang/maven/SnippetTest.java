/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import com.jcabi.log.Logger;
import com.jcabi.log.VerboseProcess;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.apache.maven.project.MavenProject;
import org.cactoos.Input;
import org.cactoos.Output;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.list.Joined;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.LengthOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.yaml.snakeyaml.Yaml;

/**
 * Test case for {@link CompileMojo}.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class SnippetTest {

    @Disabled
    @ParameterizedTest
    @MethodSource("yamlSnippets")
    @SuppressWarnings("unchecked")
    public void testFullRun(final String yml) throws Exception {
        final Yaml yaml = new Yaml();
        final Map<String, Object> map = yaml.load(
            SnippetTest.class.getResourceAsStream(
                String.format("snippets/%s", yml)
            )
        );
        final ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        final int result = SnippetTest.run(
            new InputOf(String.format("%s\n", map.get("eo"))),
            (List<String>) map.get("args"),
            new InputOf(map.get("in").toString()),
            new OutputTo(stdout)
        );
        MatcherAssert.assertThat(result, Matchers.equalTo(map.get("exit")));
        Logger.info(this, "Stdout: \"%s\"", stdout.toString());
        for (final String ptn : (Iterable<String>) map.get("out")) {
            MatcherAssert.assertThat(
                stdout.toString(),
                Matchers.matchesPattern(
                    Pattern.compile(ptn, Pattern.DOTALL | Pattern.MULTILINE)
                )
            );
        }
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Collection<String> yamlSnippets() {
        return new ListOf<>(
            "ifthenelse.yaml",
            "fibo.yaml",
            "simple.yaml"
        );
    }

    /**
     * Compile EO to Java and run.
     * @param code EO sources
     * @param args Command line arguments
     * @param stdin The input
     * @param stdout Where to put stdout
     * @return All Java code
     * @throws Exception If fails
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    @SuppressWarnings("unchecked")
    private static int run(final Input code, final List<String> args,
        final Input stdin, final Output stdout) throws Exception {
        final Path temp = Files.createTempDirectory("eo");
        final Path src = temp.resolve("src");
        new Save(code, src.resolve("code.eo")).save();
        final Path target = temp.resolve("target");
        final Path generated = temp.resolve("generated");
        final MavenProject project = new MavenProjectStub();
        new Mojo<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("sourcesDir", src.toFile())
            .execute();
        new Mojo<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .execute();
        new Mojo<>(CompileMojo.class)
            .with("project", project)
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .execute();
        final Path classes = temp.resolve("classes");
        classes.toFile().mkdir();
        final String cpath = String.format(
            ".:%s",
            System.getProperty(
                "runtime.jar",
                Paths.get(System.getProperty("user.home")).resolve(
                    String.format(
                        ".m2/repository/org/eolang/eo-runtime/%s/eo-runtime-%1$s.jar",
                        "1.0-SNAPSHOT"
                    )
                ).toString()
            )
        );
        Files.walk(generated)
            .filter(file -> !file.toFile().isDirectory())
            .forEach(file -> SnippetTest.javac(file, classes, cpath));
        final Process proc = new ProcessBuilder()
            .command(
                new Joined<>(
                    new ListOf<>(
                        "java",
                        "-cp",
                        cpath,
                        "org.eolang.phi.Main"
                    ),
                    args
                )
            )
            .directory(classes.toFile())
            .redirectErrorStream(true)
            .start();
        new LengthOf(
            new TeeInput(
                stdin,
                new OutputTo(proc.getOutputStream())
            )
        ).value();
        try (VerboseProcess vproc = new VerboseProcess(proc)) {
            new LengthOf(
                new TeeInput(
                    new InputOf(vproc.stdout()),
                    stdout
                )
            ).value();
        }
        return 0;
    }

    /**
     * Compile .java to .class and return the output.
     *
     * @param file The java file
     * @param dir Destination directory
     * @param cpath Classpath
     */
    private static String javac(final Path file, final Path dir,
        final String cpath) {
        final ProcessBuilder proc = new ProcessBuilder()
            .command(
                "javac",
                file.getFileName().toString(),
                "-d",
                dir.toString(),
                "-cp",
                cpath
            )
            .directory(file.getParent().toFile())
            .redirectErrorStream(true);
        try (VerboseProcess vproc = new VerboseProcess(proc)) {
            return vproc.stdout();
        }
    }

}
