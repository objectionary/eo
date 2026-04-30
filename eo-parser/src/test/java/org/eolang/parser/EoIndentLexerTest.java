/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.io.IOException;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link EoIndentLexer}.
 * @since 0.1.0
 */
final class EoIndentLexerTest {

    @Test
    void emitsTabWithCorrectName() throws IOException {
        MatcherAssert.assertThat(
            "We expect the token to be a tab indentation with name 'TAB'",
            EoIndentLexer.fromText(
                new TextOf(String.format("%n  "))
            ).getAllTokens().get(1).getText(),
            Matchers.equalTo("TAB")
        );
    }

    @Test
    void emitsUntabWithCorrectName() throws IOException {
        MatcherAssert.assertThat(
            "We expect the token to be an untab indentation with name 'UNTAB'",
            EoIndentLexer.fromText(
                new TextOf(String.format("%n  %n  %n"))
            ).getAllTokens().get(3).getText(),
            Matchers.equalTo("UNTAB")
        );
    }

    @Test
    void emitsTab() throws IOException {
        final EoIndentLexer lexer = EoIndentLexer.fromText(new TextOf(String.format("%n  ")));
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
            EoIndentLexer.fromText(new TextOf("")).getGrammarFileName(),
            Matchers.is("Eo.g4")
        );
    }

    @Test
    void emitsTabWhenEmptyLine() throws IOException {
        final EoIndentLexer lexer = EoIndentLexer.fromText(new TextOf(String.format("%n%n  ")));
        lexer.nextToken();
        MatcherAssert.assertThat(
            "We expect tab indentation to be emitted right after the first new line symbol",
            lexer.nextToken().getType(),
            Matchers.is(EoParser.TAB)
        );
    }

    @Test
    void emitsUntab() throws IOException {
        final EoIndentLexer lexer = EoIndentLexer.fromText(
            new TextOf(String.format("%n  %n  %n"))
        );
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
            EoIndentLexer.fromText(new TextOf("")).nextToken().getType(),
            Matchers.is(EoParser.EOF)
        );
    }

    @Test
    void emitsTabWithCorrectLine() throws IOException {
        MatcherAssert.assertThat(
            "We expect the token to be a tab indentation with line 2",
            EoIndentLexer.fromText(
                new TextOf(String.format("1.add 1 > x%n  (1.add 1) > y"))
            ).getAllTokens()
                .stream().filter(token -> token.getType() == EoParser.TAB)
                .findFirst()
                .orElseThrow()
                .getLine(),
            Matchers.equalTo(2)
        );
    }
}
