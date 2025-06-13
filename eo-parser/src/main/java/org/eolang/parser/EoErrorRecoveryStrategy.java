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
import org.antlr.v4.runtime.misc.IntervalSet;

/**
 * Custom error recovery strategy for EO parser.
 * When encountering errors in object declarations, skip to the next
 * object at the same indentation level to allow parsing to continue.
 *
 * @since 0.1
 */
final class EoErrorRecoveryStrategy extends DefaultErrorStrategy {

    @Override
    public void recover(final Parser recognizer, final RecognitionException exc) {
        // Check if we're in a context where we should attempt smart recovery
        final String rule = recognizer.getRuleInvocationStack().get(0);
        final String[] names = recognizer.getRuleNames();
        
        if (names[EoParser.RULE_bound].equals(rule) || 
            names[EoParser.RULE_object].equals(rule)) {
            
            // Skip to the next valid construct at the same indentation level
            this.skipToNextObjectAtSameLevel(recognizer);
        } else {
            super.recover(recognizer, exc);
        }
    }

    /**
     * Skip tokens until we find the start of the next object at the same indentation level.
     * @param recognizer The parser
     */
    private void skipToNextObjectAtSameLevel(final Parser recognizer) {
        final TokenStream tokens = recognizer.getInputStream();
        int index = recognizer.getCurrentToken().getTokenIndex();
        
        // Skip tokens until we find a pattern that looks like the start of a new object
        // or until we reach the end of the current indentation block
        while (index < tokens.size()) {
            final Token token = tokens.get(index);
            
            if (token.getType() == Token.EOF) {
                break;
            }
            
            // If we find UNTAB, we've reached the end of this indentation level
            if (token.getType() == EoParser.UNTAB) {
                break;
            }
            
            // Look for patterns that indicate the start of a new object:
            // EOL followed by LSQ (start of voids like "[] > name")
            if (token.getType() == EoParser.EOL && index + 1 < tokens.size()) {
                final Token next = tokens.get(index + 1);
                if (next.getType() == EoParser.LSQ) {
                    // Found the start of what looks like a new object
                    // Seek to this position and let parsing continue
                    recognizer.getInputStream().seek(index);
                    return;
                }
            }
            
            index++;
        }
        
        // If we didn't find a good recovery point, seek to the end of what we scanned
        if (index < tokens.size()) {
            recognizer.getInputStream().seek(index);
        }
    }

    @Override
    protected void consumeUntil(final Parser recognizer, final IntervalSet set) {
        // Enhanced token consumption that's aware of indentation structure
        Token token = recognizer.getCurrentToken();
        while (token.getType() != Token.EOF && !set.contains(token.getType())) {
            // Stop if we hit an UNTAB - end of current indentation level
            if (token.getType() == EoParser.UNTAB) {
                break;
            }
            recognizer.consume();
            token = recognizer.getCurrentToken();
        }
    }
}