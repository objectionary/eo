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
package org.eolang.parser;

import java.io.IOException;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link EoIndentLexer}.
 *
 * @since 1.0
 */
final class EoIndentLexerTest {
    @Test
    void emitsTab() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(
            new TextOf("\n  ")
        );
        lexer.nextToken();
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                EoParser.TAB
            )
        );
    }

    @Test
    void ensuresGrammarFile() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(
            new TextOf("")
        );
        MatcherAssert.assertThat(
            lexer.getGrammarFileName(),
            Matchers.is(
                "Eo.g4"
            )
        );
    }

    @Test
    void emitsTabWhenEmptyLine() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(
            new TextOf("\n\n  ")
        );
        lexer.nextToken();
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                EoParser.TAB
            )
        );
    }

    @Test
    void emitsUntab() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(
            new TextOf("\n  \n  \n")
        );
        lexer.nextToken();
        lexer.nextToken();
        lexer.nextToken();
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                EoParser.UNTAB
            )
        );
    }

    @Test
    void readsEmptyString() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(
            new TextOf("")
        );
        MatcherAssert.assertThat(
            lexer.nextToken().getType(),
            Matchers.is(
                EoParser.EOF
            )
        );
    }
}
