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

import com.jcabi.xml.XML;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link JavaFiles}.
 *
 * @since 0.29
 */
class JavaFilesTest {

    /**
     * Parsed eo program from resources.
     */
    private XML xmir;

    @BeforeEach
    void setUp() throws IOException {
        this.xmir = new EoSyntax(
            "sum.eo",
            new ResourceOf("org/eolang/maven/sum.eo")
        ).parsed();
    }

    @Test
    void convertsXmirtoJavaSuccessfully(@TempDir final Path temp) throws Exception {
        new HmBase(temp.resolve("xml")).save(
            new Xsline(TranspileMojo.TRAIN).pass(this.xmir).toString(),
            Paths.get("sum.xmir")
        );
        MatcherAssert.assertThat(
            new TextOf(
                new InputOf(
                    new JavaFiles(
                        temp.resolve("xml").resolve("sum.xmir"),
                        temp.resolve("java")
                    ).save().get(0)
                )
            ).asString(),
            Matchers.containsString("final class EOsum extends PhDefault")
        );
    }

    @Test
    void convertsXmirtoJavaWithoutJavaClasses(@TempDir final Path temp) throws Exception {
        new HmBase(temp.resolve("xml")).save(
            this.xmir.toString(),
            Paths.get("sum.xmir")
        );
        MatcherAssert.assertThat(
            new JavaFiles(
                temp.resolve("xml").resolve("sum.xmir"),
                temp.resolve("java")
            ).save(),
            Matchers.empty()
        );
    }
}
