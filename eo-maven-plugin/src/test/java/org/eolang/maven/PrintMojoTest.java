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

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Walk;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link PrintMojo}.
 * @since 0.33.0
 * @todo #2758:30min To test that PrintMojo prints EO in different notations depends
 *  on given flag. When {@link org.eolang.parser.xmir.XmirReversed} and
 *  {@link org.eolang.parser.xmir.Xmir.Default} work correctly and prints xmirs in
 *  corresponding notations we need to check that PrintMojo uses them correctly
 *  depends on given flag {@link PrintMojo#printReversed}.
 */
final class PrintMojoTest {
    @Test
    void printsSuccessfully(@TempDir final Path temp) throws Exception {
        final Home home = new HmBase(temp);
        final Path resources = new File("src/test/resources/org/eolang/maven/print")
            .toPath();
        final Collection<Path> walk = new Walk(resources);
        for (final Path source : walk) {
            home.save(new TextOf(source), source);
        }
        final Path output = temp.resolve("output");
        final Path sources = temp.resolve(resources);
        new FakeMaven(temp)
            .with("printSourcesDir", sources.toFile())
            .with("printOutputDir", output.toFile())
            .execute(new FakeMaven.Print())
            .result();
        for (final Path source : walk) {
            final String src =  resources.relativize(source).toString()
                .replace(".xmir", ".eo");
            MatcherAssert.assertThat(
                String.format(
                    "File with name %s should have existed in output directory, but it didn't",
                    src
                ),
                Files.exists(output.resolve(Paths.get(src))),
                Matchers.is(true)
            );
        }
    }
}
