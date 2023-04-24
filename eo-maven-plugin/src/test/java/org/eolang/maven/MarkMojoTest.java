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
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.maven.tojos.ForeignTojos;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link MarkMojo}.
 *
 * @since 0.11
 */
final class MarkMojoTest {

    @Test
    void extendsForeignWithNewObjects(@TempDir final Path temp) throws IOException {
        MarkMojoTest.source(temp);
        final FakeMaven maven = new FakeMaven(temp);
        maven.execute(MarkMojo.class);
        MatcherAssert.assertThat(
            maven.foreignTojos()
                .all()
                .iterator()
                .next()
                .version(),
            Matchers.equalTo("0.1.8")
        );
    }

    @Test
    void updatesVersionIfItExists(@TempDir final Path temp) throws IOException {
        MarkMojoTest.source(temp);
        final FakeMaven maven = new FakeMaven(temp);
        final ForeignTojos foreign = maven.foreignTojos();
        foreign.add("foo.bar")
            .withVersion("*.*.*");
        maven.execute(MarkMojo.class);
        MatcherAssert.assertThat(
            foreign.all().iterator().next().version(),
            Matchers.equalTo("0.1.8")
        );
        MatcherAssert.assertThat(
            foreign.size(),
            Matchers.equalTo(1)
        );
    }

    private static void source(final Path temp) throws IOException {
        new Home(temp.resolve("target").resolve(ResolveMojo.DIR))
            .save(
                "hi",
                Paths.get(String.format("foo/hello/-/0.1.8/%s/foo/bar.eo", CopyMojo.DIR))
            );
    }
}
