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
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XaxStory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link TranspileMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(MktmpResolver.class)
final class TranspileMojoTest {

    /**
     * Test eo program from resources.
     */
    private String program;

    /**
     * Traspiled to java eo program from resources.
     */
    private String compiled;

    @BeforeEach
    void setUp() throws Exception {
        this.program = new TextOf(new ResourceOf("org/eolang/maven/mess.eo")).asString();
        this.compiled = "target/generated/EOorg/EOeolang/EOexamples/EOmessTest.java";
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/pre/", glob = "**.yaml")
    void createsPreStylesheets(final String yaml) {
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            new XaxStory(yaml),
            Matchers.is(true)
        );
    }

    @Test
    void recompilesIfModified(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(this.compiled);
        final long before = java.toFile().lastModified();
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            res.get("foo/x/main.eo").toFile().setLastModified(before + 1L),
            Matchers.is(true)
        );
        maven.execute(new FakeMaven.Transpile());
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            java.toFile().lastModified(),
            Matchers.greaterThan(before)
        );
    }

    @Test
    void recompilesIfExpired(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(this.compiled);
        final Path xmir = res.get(
            String.format("target/%s/foo/x/main.xmir", TranspileMojo.DIR)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            java.toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            xmir.toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            java.toFile().setLastModified(0L),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            xmir.toFile().setLastModified(0L),
            Matchers.is(true)
        );
        final long before = java.toFile().lastModified();
        maven.execute(TranspileMojo.class);
        final long after = java.toFile().lastModified();
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            after,
            Matchers.greaterThan(0L)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            before,
            Matchers.not(Matchers.equalTo(after))
        );
    }

    @Test
    void doesNotRetranspileIfNotModified(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Path java = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result().get(this.compiled);
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            java.toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            java.toFile().setLastModified(0L),
            Matchers.is(true)
        );
        maven.execute(TranspileMojo.class);
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            java.toFile().lastModified(),
            Matchers.is(0L)
        );
    }

    @Test
    void transpilesSimpleEoProgram(@Mktmp final Path temp) throws Exception {
        final Path src = Paths.get("../eo-runtime/src/main/eo/org/eolang/tuple.eo");
        final Map<String, Path> res = new FakeMaven(temp)
            .withProgram(src)
            .execute(new FakeMaven.Transpile())
            .result();
        final String java = "target/generated/EOorg/EOeolang/EOtuple.java";
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            res, Matchers.hasKey(java)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            new TextOf(res.get(java)).asString(),
            Matchers.containsString("class EOtuple")
        );
    }

    @Test
    void transpilesSeveralEoProgramsInParallel(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int programs = 30;
        for (int prog = 0; prog < programs; ++prog) {
            maven.withProgram(this.program);
        }
        maven.execute(new FakeMaven.Transpile()).result();
        MatcherAssert.assertThat(
            "All programs must be transpiled",
            Files.list(
                maven.generatedPath()
                    .resolve("EOorg")
                    .resolve("EOeolang")
                    .resolve("EOexamples")
            ).count(),
            Matchers.equalTo(4L)
        );
    }

    @Test
    void transpilesSourcesForDifferentScopesWithoutIntersections(
        @Mktmp final Path temp
    ) throws IOException {
        final Path target = temp.resolve("target");
        final Path sources = target.resolve("generated-sources");
        final Path tests = target.resolve("generated-test-sources");
        final FakeMaven maven = new FakeMaven(temp);
        maven
            .with("generatedDir", sources.toFile())
            .with("targetDir", target.resolve("eo-sources").toFile())
            .withHelloWorld()
            .execute(new FakeMaven.Transpile());
        maven
            .with("scope", "test")
            .with("generatedDir", tests.toFile())
            .with("targetDir", target.resolve("eo-test-sources").toFile())
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile());
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            maven.foreign().size(),
            Matchers.equalTo(2)
        );
        final Set<String> intersection = TranspileMojoTest.classes(tests);
        intersection.retainAll(TranspileMojoTest.classes(sources));
        MatcherAssert.assertThat(
            "Both class paths should not intersect and don't have to have common classes",
            intersection,
            Matchers.empty()
        );
    }

    /**
     * Get all classes in directory.
     * @param root Directory to get classes from.
     * @return Set of classes.
     * @throws IOException If fails.
     */
    private static Set<String> classes(final Path root) throws IOException {
        try (Stream<Path> walk = Files.walk(root)) {
            return walk.filter(TranspileMojoTest::isJava)
                .map(TranspileMojoTest::filename)
                .collect(Collectors.toSet());
        }
    }

    /**
     * Is java file.
     * @param path Path to check.
     * @return True if path is java file.
     */
    private static boolean isJava(final Path path) {
        return Files.isRegularFile(path) && path.toString().endsWith(".java");
    }

    /**
     * Get filename.
     * @param path Path to get filename from.
     * @return Filename.
     */
    private static String filename(final Path path) {
        return path.getFileName().toString();
    }
}
