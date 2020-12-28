/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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

import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.apache.maven.project.MavenProject;
import org.cactoos.Input;
import org.cactoos.Output;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.LengthOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
        final ByteArrayOutputStream stderr = new ByteArrayOutputStream();
        final int result = this.run(
            new InputOf(String.format("%s\n", map.get("eo"))),
            new InputOf(map.get("in").toString()),
            new OutputTo(stdout),
            new OutputTo(stderr)
        );
        MatcherAssert.assertThat(result, Matchers.equalTo(map.get("exit")));
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Collection<String> yamlSnippets() {
        return new ListOf<>(
            "simple.yaml"
//            "leap-year.yaml"
        );
    }

    /**
     * Compile EO to Java and run.
     * @param code EO sources
     * @param stdin The input
     * @return All Java code
     * @throws Exception If fails
     */
    private int run(final Input code, final Input stdin,
        final Output stdout, final Output stderr) throws Exception {
        final Path temp = Files.createTempDirectory("eo");
        final Path src = temp.resolve("src");
        new LengthOf(
            new TeeInput(
                code,
                new OutputTo(src.resolve("code.eo"))
            )
        ).value();
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
        final Path javac = temp.resolve("javac");
        return 0;
    }

}
