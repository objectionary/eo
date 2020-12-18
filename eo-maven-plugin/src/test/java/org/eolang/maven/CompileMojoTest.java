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

import com.jcabi.log.Logger;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.ResourceOf;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.LengthOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link CompileMojo}.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class CompileMojoTest {

    @Test
    public void testSimpleCompilation() throws Exception {
        final String java = this.compile(
            new ResourceOf("org/eolang/maven/mess.eo"),
            "EOmess.java"
        );
        MatcherAssert.assertThat(java, Matchers.containsString("class EOmess"));
    }

    @Test
    public void testRealCompilation() throws Exception {
        final String java = this.compile(
            new ResourceOf("org/eolang/maven/array.eo"),
            "EOarray$EOmap.java"
        );
        MatcherAssert.assertThat(java, Matchers.containsString("class"));
    }

    /**
     * Compile EO to Java.
     * @param code EO sources
     * @param file The file to return
     * @return All Java code
     * @throws Exception If fails
     */
    private String compile(final Input code,
        final String file) throws Exception {
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
        new Mojo<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("sourcesDir", src.toFile())
            .execute();
        new Mojo<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .execute();
        new Mojo<>(CompileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .execute();
        final Path java = generated.resolve(file);
        MatcherAssert.assertThat(
            Files.exists(java),
            Matchers.is(true)
        );
        final String out = new TextOf(new InputOf(java)).asString();
        Logger.info(this, "Java output:\n%s", out);
        return out;
    }

}
