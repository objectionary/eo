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
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.name.OnVersioned;
import org.eolang.maven.tojos.ForeignTojos;
import org.eolang.maven.util.HmBase;
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
    /**
     * Version.
     */
    private static final String VERSION = "0.28.0";

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
            Matchers.equalTo(MarkMojoTest.VERSION)
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
            Matchers.equalTo(MarkMojoTest.VERSION)
        );
        MatcherAssert.assertThat(
            foreign.size(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void extendsTojosWithVersionedOne(@TempDir final Path temp) throws IOException {
        MarkMojoTest.source(temp);
        final ForeignTojos tojos = new FakeMaven(temp)
            .with("withVersions", true)
            .execute(MarkMojo.class)
            .foreignTojos();
        final ObjectName object = new OnVersioned("foo.bar", "6a70071");
        MatcherAssert.assertThat(
            String.format(
                "Tojos should have contained versioned object %s after extending, but they didn't",
                object
            ),
            tojos.contains(object),
            Matchers.is(true)
        );
    }

    private static void source(final Path temp) throws IOException {
        new HmBase(temp.resolve("target").resolve(ResolveMojo.DIR)).save(
            "hi",
            Paths.get(
                String.format(
                    "foo/hello/-/%s/%s/foo/bar.eo", MarkMojoTest.VERSION, CopyMojo.DIR
                )
            )
        );
    }
}
