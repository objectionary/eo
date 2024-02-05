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
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Stack;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.Token;
import org.cactoos.Text;
import org.cactoos.io.InputStreamOf;

/**
 * Indentation-aware Lexer.
 *
 * @since 1.0
 */
public final class EoIndentLexer extends EoLexer {
    /**
     * Generated tokes.
     */
    private final Deque<Token> tokens;

    /**
     * Indentation level.
     */
    private final Deque<Integer> indent;

    /**
     * Spaces.
     */
    private final Stack<String> spaces;

    /**
     * Ctor.
     * @param txt Source code.
     * @throws IOException If fails.
     */
    public EoIndentLexer(final Text txt) throws IOException {
        this(CharStreams.fromStream(new InputStreamOf(txt)));
    }

    /**
     * Ctor.
     * @param stream Char stream.
     */
    private EoIndentLexer(final CharStream stream) {
        super(stream);
        this.indent = new LinkedList<>(
            Collections.singletonList(0)
        );
        this.tokens = new LinkedList<>();
        this.spaces = new Stack<>();
    }

    @Override
    public Token nextToken() {
        if (this.tokens.isEmpty()) {
            this.lookAhead();
        }
        return this.tokens.poll();
    }

    /**
     * Look for upcoming token.
     */
    private void lookAhead() {
        final Token current = this.getToken();
        final Token next = super.nextToken();
        if (next.getType() == EoParser.EOL
            && (current == null || current.getType() != EoParser.EOL)) {
            this.spaces.push(this.textSpaces());
            this.tokens.offer(next);
        } else if (current != null && current.getType() == EoParser.EOL
            && next.getType() != EoParser.EOL) {
            this.handleTabs(this.spaces.pop().length() / 2, next);
        } else if (current != null && current.getType() == EoParser.EOL
            && next.getType() == EoParser.EOL) {
            this.spaces.push(this.textSpaces());
            this.handleTabs(this.spaces.peek().length() / 2, next);
        } else {
            this.tokens.offer(next);
        }
    }

    /**
     * Calculate shifts and emit corresponding token.
     * @param tabs Current amount of tabs.
     * @param next Next token.
     */
    private void handleTabs(final int tabs, final Token next) {
        final int last = this.indent.peekLast();
        final int shift = tabs - last;
        if (shift < 0) {
            this.emitDedent(-shift);
        } else if (shift > 0) {
            this.emitIndent(shift);
        }
        this.indent.add(tabs);
        this.tokens.add(next);
    }

    /**
     * Get string with spaces from current text.
     * @return Spaces from text.
     */
    private String textSpaces() {
        return this.getText().replaceAll("[\r\n]", "");
    }

    /**
     * Indent.
     *
     * @param shift Number of tabs
     */
    private void emitIndent(final int shift) {
        for (int idx = 0; idx < shift; ++idx) {
            this.emitToken(EoParser.TAB);
        }
    }

    /**
     * De-indent.
     *
     * @param shift Number of un-tabs.
     */
    private void emitDedent(final int shift) {
        for (int idx = 0; idx < shift; ++idx) {
            this.emitToken(EoParser.UNTAB);
        }
    }

    /**
     * Emit token at the next line.
     *
     * @param type Type.
     */
    private void emitToken(final int type) {
        final CommonToken tkn = new CommonToken(type, this.getRuleNames()[type]);
        tkn.setLine(this.getLine() + 1);
        this.tokens.offer(tkn);
    }
}
