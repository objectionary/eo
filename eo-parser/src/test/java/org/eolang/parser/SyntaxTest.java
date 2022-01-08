/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.cactoos.io.DeadOutput;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link Xsline}.
 *
 * @since 0.1
 */
public final class SyntaxTest {

    @Test
    public void compilesSimpleCode() throws Exception {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Syntax syntax = new Syntax(
            "test-1",
            new ResourceOf("org/eolang/parser/fibonacci.eo"),
            new OutputTo(baos)
        );
        syntax.parse();
        MatcherAssert.assertThat(
            XhtmlMatchers.xhtml(
                new String(baos.toByteArray(), StandardCharsets.UTF_8)
            ),
            XhtmlMatchers.hasXPath(
                "/program[@name='test-1']",
                "/program[@ms and @time and @version]",
                "/program/listing",
                "/program/metas/meta[head='meta2']",
                "/program/objects/o[@name='fibo']"
            )
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "1 > x\n2 > y",
        "1 > x\r\n2 > y",
        "1 > x\r\n\r\n2 > y",
        "1 > x\n2 > y\n",
        "1 > x\n\n2 > y",
        "[]",
        "[] > x",
        "a b c > x\n  x ^ > @",
        "[] > x\n  x ^ > @"
    })
    public void shouldBeParsable(final String code) throws Exception {
        final Syntax syntax = new Syntax(
            "test-2",
            new InputOf(code),
            new DeadOutput()
        );
        Assertions.assertDoesNotThrow(
            syntax::parse
        );
    }

    @Test
    public void failsWithDoubleNewLine() throws Exception {
        final Syntax syntax = new Syntax(
            "test-3",
            new InputOf("1 > x\n\n\n2 > y"),
            new DeadOutput()
        );
        Assertions.assertThrows(
            ParsingException.class,
            syntax::parse
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "...",
        ">",
        "",
        "\n\n\n\n",
        "<",
        "a>b",
        "@ > []",
        "^ > ^",
        "this < code is definitely > wrong",
        "[] > x\n x ^ > @"
    })
    public void failsOnBrokenSyntax(final String code) throws IOException {
        final Syntax syntax = new Syntax(
            "test-it",
            new InputOf(code),
            new DeadOutput()
        );
        Assertions.assertThrows(
            ParsingException.class,
            syntax::parse
        );
    }

}
