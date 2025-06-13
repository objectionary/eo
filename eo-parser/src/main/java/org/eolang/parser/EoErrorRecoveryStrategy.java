/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;

/**
 * Custom error recovery strategy for EO parser.
 * When encountering errors in object declarations, skip to the next
 * object at the same indentation level to allow parsing to continue.
 *
 * @since 0.1
 */
final class EoErrorRecoveryStrategy extends DefaultErrorStrategy {

    @Override
    public void recover(
        final Parser recognizer,
        final RecognitionException exc
    ) {
        final String rule = recognizer.getRuleInvocationStack().get(0);
        final String[] names = recognizer.getRuleNames();
        if (names[EoParser.RULE_bound].equals(rule)
            || names[EoParser.RULE_object].equals(rule)) {
            EoErrorRecoveryStrategy.skipToNextObjectAtSameLevel(recognizer);
        } else {
            super.recover(recognizer, exc);
        }
    }

    /**
     * Skip tokens until we find the start of the next object
     * at the same indentation level.
     * @param recognizer The parser
     */
    private static void skipToNextObjectAtSameLevel(final Parser recognizer) {
        final TokenStream tokens = recognizer.getInputStream();
        int index = recognizer.getCurrentToken().getTokenIndex();
        while (index < tokens.size()) {
            final Token token = tokens.get(index);
            if (token.getType() == Token.EOF) {
                break;
            }
            if (token.getType() == EoParser.UNTAB) {
                break;
            }
            if (token.getType() == EoParser.EOL && index + 1 < tokens.size()) {
                final Token next = tokens.get(index + 1);
                if (next.getType() == EoParser.LSQ) {
                    recognizer.getInputStream().seek(index);
                    return;
                }
            }
            index = index + 1;
        }
        if (index < tokens.size()) {
            recognizer.getInputStream().seek(index);
        }
    }
}
