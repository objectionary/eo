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
package org.eolang.parser;

import java.io.IOException;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link EoLexer}.
 *
 * @since 1.0
 */
final class EoLexerTest {
    @Test
    void emitsTab() throws IOException {
        final EoLexer lexer = new EoLexer(
            new TextOf("\n  ")
        );
        lexer.nextToken();
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                ProgramParser.TAB
            )
        );
    }

    @Test
    void ensuresGrammarFile() throws IOException {
        final EoLexer lexer = new EoLexer(
            new TextOf("")
        );
        MatcherAssert.assertThat(
            lexer.getGrammarFileName(),
            Matchers.is(
                "Program.g4"
            )
        );
    }

    @Test
    void emitsTabWhenEmptyLine() throws IOException {
        final EoLexer lexer = new EoLexer(
            new TextOf("\n\n  ")
        );
        lexer.nextToken();
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                ProgramParser.TAB
            )
        );
    }

    @Test
    void emitsUntab() throws IOException {
        final EoLexer lexer = new EoLexer(
            new TextOf("\n  \n")
        );
        lexer.nextToken();
        lexer.nextToken();
        lexer.nextToken();
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                ProgramParser.UNTAB
            )
        );
    }

    @Test
    void readsEmptyString() throws IOException {
        final EoLexer lexer = new EoLexer(
            new TextOf("")
        );
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                ProgramParser.EOF
            )
        );
    }
}
