/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link CompilerOriginal}.
 *
 * @since 0.1
 */
public final class CompilerOriginalTest {

    @Test
    public void simpleCompile(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("test.eo");
        new Save("+package foo.bar\n\n[x] > hello\n", src).save();
        final Path xml = temp.resolve("xml");
        new Parsing(src, temp.resolve("1")).into(xml, "foo.bar.hello");
        final Path target = temp.resolve("target");
        final Path pre = temp.resolve("pre");
        final Path generated = temp.resolve("generated");
        new CompilerOriginal(target, pre).compile(
            xml.resolve(String.format("%s/foo/bar/hello.eo.xml", ParseMojo.DIR)),
            generated
        );
        MatcherAssert.assertThat(
            Files.exists(generated.resolve("EOfoo/EObar/EOhello.java")),
            Matchers.is(true)
        );
    }

}
