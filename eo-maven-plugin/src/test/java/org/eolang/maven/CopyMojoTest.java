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

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link CopyMojo}.
 *
 * @since 0.1
 */
final class CopyMojoTest {

    @Test
    void copiesSources(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("src");
        final Path classes = temp.resolve("classes");
        new HmBase(src).save(
            "+rt foo:0.0.0\n\n[args] > main\n  \"0.0.0\" > @\n",
            Paths.get("foo/main.eo")
        );
        final String ver = "1.1.1";
        new FakeMaven(temp)
            .with("sourcesDir", src.toFile())
            .with("outputDir", classes.toFile())
            .with("skip", false)
            .with("version", ver)
            .execute(CopyMojo.class);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
            new HmBase(classes).exists(classes.relativize(out)),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new TextOf(new HmBase(classes).load(classes.relativize(out))).asString(),
            Matchers.allOf(
                Matchers.containsString("+rt foo:"),
                Matchers.containsString("0.0.0"),
                Matchers.containsString(ver)
            )
        );
    }

    @Test
    void skipsCopyMojo(@TempDir final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        new FakeMaven(temp)
            .withProgram(
                "+rt foo:0.0.0",
                "",
                "",
                "[args] > main",
                "  \"0.0.0\" > @"
            )
            .with("sourcesDir", temp.toFile())
            .with("outputDir", temp.resolve("classes").toFile())
            .with("skip", true)
            .with("version", "1.1.1")
            .execute(CopyMojo.class);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
            new HmBase(classes).exists(classes.relativize(out)),
            Matchers.is(false)
        );
    }

}
