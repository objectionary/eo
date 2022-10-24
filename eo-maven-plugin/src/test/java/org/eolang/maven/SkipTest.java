/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import org.cactoos.io.InputOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for skip option in {@link SafeMojo}.
 *
 * @since 0.22
 */
class SkipTest {

    @Test
    void testExecutedPullMojo(@TempDir final Path temp) {
        final Path target = temp.resolve("target");
        this.executePullMojo(temp, target, false);
        MatcherAssert.assertThat(
            new Home(target).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.eo",
                        PullMojo.DIR
                    )
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void testSkippedPullMojo(@TempDir final Path temp) {
        final Path target = temp.resolve("target");
        this.executePullMojo(temp, target, true);
        MatcherAssert.assertThat(
            !new Home(target).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.eo",
                        PullMojo.DIR
                    )
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void testSkippedCopyMojo(@TempDir final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        this.executeCopyMojo(temp, classes, true);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
            !new Home(classes).exists(classes.relativize(out)),
            Matchers.is(true)
        );
    }

    @Test
    void testExecutedCopyMojo(@TempDir final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        this.executeCopyMojo(temp, classes, false);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
            new Home(classes).exists(classes.relativize(out)),
            Matchers.is(true)
        );
    }

    private void executePullMojo(
        @TempDir final Path temp,
        final Path target,
        final boolean skip
    ) {
        final Path foreign = temp.resolve("eo-foreign.json");
        Catalogs.INSTANCE.make(foreign, "json")
            .add("org.eolang.io.stdout")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_VERSION, "*.*.*");
        new Moja<>(PullMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "json")
            .with("skip", skip)
            .with(
                "objectionary",
                (Objectionary) input -> new InputOf("[] > hello\n")
            )
            .execute();
    }

    private void executeCopyMojo(
        @TempDir final Path temp,
        final Path classes,
        final boolean skip
    ) throws IOException {
        final Path src = temp.resolve("src");
        new Home(src).save(
            "+rt foo:0.0.0\n\n[args] > main\n  \"0.0.0\" > @\n",
            Paths.get("foo/main.eo")
        );
        final String ver = "1.1.1";
        new Moja<>(CopyMojo.class)
            .with("sourcesDir", src.toFile())
            .with("outputDir", classes.toFile())
            .with("version", ver)
            .with("skip", skip)
            .execute();
    }
}
