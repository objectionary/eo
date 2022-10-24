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

import java.nio.file.Path;
import java.nio.file.Paths;
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
    void extendForeignWithNewObjects(@TempDir final Path temp) throws Exception {
        final Path bins = temp.resolve(ResolveMojo.DIR);
        new Home(bins).save(
            "hi",
            Paths.get(String.format("foo/hello/-/0.1.8/%s/foo/bar.eo", CopyMojo.DIR))
        );
        final Path foreign = temp.resolve("placed.json");
        new Moja<>(MarkMojo.class)
            .with("targetDir", temp.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            Catalogs.INSTANCE.make(foreign).select(t -> true)
                .iterator().next()
                .get(AssembleMojo.ATTR_VERSION),
            Matchers.equalTo("0.1.8")
        );
    }

}
