/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XaxStory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link TranspileMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
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
            new XaxStory(yaml),
            Matchers.is(true)
        );
    }

    @Test
    void recompilesIfModified(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(this.compiled);
        final long before = java.toFile().lastModified();
        MatcherAssert.assertThat(
            res.get("foo/x/main.eo").toFile().setLastModified(before + 1L),
            Matchers.is(true)
        );
        maven.execute(TranspileMojo.class);
        MatcherAssert.assertThat(
            java.toFile().lastModified(),
            Matchers.greaterThan(before)
        );
    }

    @Test
    void recompilesIfExpired(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(this.compiled);
        final Path xmir = res.get("target/6-transpile/foo/x/main.xmir");
        MatcherAssert.assertThat(java.toFile(), FileMatchers.anExistingFile());
        MatcherAssert.assertThat(xmir.toFile(), FileMatchers.anExistingFile());
        MatcherAssert.assertThat(java.toFile().setLastModified(0L), Matchers.is(true));
        MatcherAssert.assertThat(xmir.toFile().setLastModified(0L), Matchers.is(true));
        final long before = java.toFile().lastModified();
        maven.execute(TranspileMojo.class);
        final long after = java.toFile().lastModified();
        MatcherAssert.assertThat(after, Matchers.greaterThan(0L));
        MatcherAssert.assertThat(before, Matchers.not(Matchers.equalTo(after)));
    }

    @Test
    void doesNotRetranspileIfNotModified(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Path java = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result().get(this.compiled);
        MatcherAssert.assertThat(java.toFile(), FileMatchers.anExistingFile());
        MatcherAssert.assertThat(java.toFile().setLastModified(0L), Matchers.is(true));
        maven.execute(TranspileMojo.class);
        MatcherAssert.assertThat(java.toFile().lastModified(), Matchers.is(0L));
    }

    @Test
    void transpilesSimpleEoProgram(@TempDir final Path temp) throws Exception {
        final Path src = Paths.get("../eo-runtime/src/main/eo/org/eolang/tuple.eo");
        final Map<String, Path> res = new FakeMaven(temp)
            .withProgram(src)
            .execute(new FakeMaven.Transpile())
            .result();
        final String java = "target/generated/EOorg/EOeolang/EOtuple.java";
        MatcherAssert.assertThat(
            res, Matchers.hasKey(java)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(java)).asString(),
            Matchers.containsString("class EOtuple")
        );
    }

    @Test
    void transpilesSeveralEoProgramsInParallel(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int programs = 30;
        for (int prog = 0; prog < programs; ++prog) {
            maven.withProgram(this.program);
        }
        maven.execute(new FakeMaven.Transpile()).result();
        MatcherAssert.assertThat(
            Files.list(maven.generatedPath()
                .resolve("EOorg")
                .resolve("EOeolang")
                .resolve("EOexamples")
            ).count(),
            Matchers.equalTo(4L)
        );
    }
}
