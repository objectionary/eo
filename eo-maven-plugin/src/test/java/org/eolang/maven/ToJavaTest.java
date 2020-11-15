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

import com.jcabi.xml.XMLDocument;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.compiler.Program;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ToJava}.
 *
 * @since 1.0
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
public final class ToJavaTest {

    /**
     * Temp dir for tests.
     * @checkstyle VisibilityModifierCheck (4 lines)
     */
    @TempDir
    public Path temp;

    @Test
    public void compilesSimpleCodeToJava() throws Exception {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Program program = new Program(
            "test",
            new ResourceOf("org/eolang/maven/fibo.eo"),
            new OutputTo(baos)
        );
        program.compile();
        final ToJava tojava = new ToJava(
            new XMLDocument(baos.toString()),
            this.temp
        );
        tojava.compile();
        final Path file = this.temp.resolve(Paths.get("fibo.java"));
        MatcherAssert.assertThat(
            Files.exists(file),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new TextOf(new InputOf(file)).asString(),
            Matchers.containsString("public final class fibo")
        );
    }

}
