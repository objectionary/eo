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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link EoIndentLexer}.
 *
 * @since 0.1.0
 */
@SuppressWarnings("PMD.JUnit5TestShouldBePackagePrivate")
public final class EoIndentLexerTest {
    /**
     * Empty message for JUnit Assertions.
     *
     * @todo #2297:60m Replace all appearances of {@link EoIndentLexerTest#TO_ADD_MESSAGE} field in
     *  eo-parser with meaningful assert messages. Don't forget to remove
     *  {@link EoIndentLexerTest#TO_ADD_MESSAGE} field and remove public modifier from this class if
     *  no longer need.
     */
    public static final String TO_ADD_MESSAGE = "TO ADD ASSERTION MESSAGE";

    /**
     * Tests if the lexer emits a tab token with the correct name.
     * @throws IOException In case if the input is invalid.
     * @todo #3706:30min Fix the the name of the TAB token.
     *  Currently {@link EoIndentLexer} emits TAB token with the name 'ZERO' instead of 'TAB'.
     *  Fix the lexer to emit TAB token with the correct name.
     *  This problem affects lexer error messages.
     *  When this issue is fixed, remove the @Disabled annotation from the test.
     */
    @Test
    @Disabled
    void emitsTabWithCorrectName() throws IOException {
        MatcherAssert.assertThat(
            "We expect the token to be a tab indentation with name 'TAB'",
            new EoIndentLexer(new TextOf("\n  ")).getAllTokens().get(1).getText(),
            Matchers.equalTo("TAB")
        );
    }

    /**
     * Tests if the lexer emits an untab token with the correct name.
     * @throws IOException In case if the input is invalid.
     * @todo #3706:30min Fix the the name of the UNTAB token.
     *  Currently {@link EoIndentLexer} emits UNTAB token with the name 'INT' instead of 'UNTAB'.
     *  Fix the lexer to emit UNTAB token with the correct name.
     *  This problem affects lexer error messages.
     *  When this issue is fixed, remove the @Disabled annotation from the test.
     */
    @Test
    @Disabled
    void emitsUntabWithCorrectName() throws IOException {
        MatcherAssert.assertThat(
            "We expect the token to be an untab indentation with name 'UNTAB'",
            new EoIndentLexer(new TextOf("\n  \n  \n")).getAllTokens().get(3).getText(),
            Matchers.equalTo("UNTAB")
        );
    }

    @Test
    void emitsTab() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(new TextOf("\n  "));
        lexer.nextToken();
        MatcherAssert.assertThat(
            "We expect the first token to be a new line, whereas the next token is tab indentation",
            lexer.nextToken().getType(),
            Matchers.is(EoParser.TAB)
        );
    }

    @Test
    void ensuresGrammarFile() throws IOException {
        MatcherAssert.assertThat(
            "We expect to retrieve the correct grammar file name",
            new EoIndentLexer(new TextOf("")).getGrammarFileName(),
            Matchers.is("Eo.g4")
        );
    }

    @Test
    void emitsTabWhenEmptyLine() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(new TextOf("\n\n  "));
        lexer.nextToken();
        MatcherAssert.assertThat(
            "We expect tab indentation to be emitted right after the first new line symbol",
            lexer.nextToken().getType(),
            Matchers.is(EoParser.TAB)
        );
    }

    @Test
    void emitsUntab() throws IOException {
        final EoIndentLexer lexer = new EoIndentLexer(new TextOf("\n  \n  \n"));
        lexer.nextToken();
        lexer.nextToken();
        lexer.nextToken();
        MatcherAssert.assertThat(
            "We expect untab indentation token to be emitted after the second new line symbol",
            lexer.nextToken().getType(),
            Matchers.is(EoParser.UNTAB)
        );
    }

    @Test
    void readsEmptyString() throws IOException {
        MatcherAssert.assertThat(
            "We expect the lexer to return EOF token when the input is empty",
            new EoIndentLexer(new TextOf("")).nextToken().getType(),
            Matchers.is(EoParser.EOF)
        );
    }
}
