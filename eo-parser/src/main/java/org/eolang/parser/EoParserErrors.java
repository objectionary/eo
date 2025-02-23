/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.InputMismatchException;
import org.antlr.v4.runtime.NoViableAltException;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.cactoos.Text;

/**
 * Accumulates all parsing errors related to EO parser.
 * @since 0.50
 */
final class EoParserErrors extends BaseErrorListener implements Iterable<ParsingException> {

    /**
     * Errors accumulated.
     */
    private final List<ParsingException> errors;

    /**
     * The source.
     */
    private final Lines lines;

    /**
     * Ctor.
     * @param src The source in lines
     */
    EoParserErrors(final List<Text> src) {
        this(new ArrayList<>(0), new Lines(src));
    }

    /**
     * Ctor.
     * @param errors Errors accumulated
     * @param lines The source in lines
     */
    private EoParserErrors(final List<ParsingException> errors, final Lines lines) {
        this.errors = errors;
        this.lines = lines;
    }

    // @checkstyle ParameterNumberCheck (20 lines)
    // @checkstyle CyclomaticComplexityCheck (100 lines)
    // @todo #3744:30min Simplify {@link EoParserErrors#syntaxError} method.
    //  The method is too complex and has a high cognitive complexity. We need to simplify it.
    //  Don't forget to remove the @checkstyle CyclomaticComplexityCheck annotation and
    //  the @SuppressWarnings("PMD.CognitiveComplexity") annotation.
    @Override
    @SuppressWarnings("PMD.CognitiveComplexity")
    public void syntaxError(
        final Recognizer<?, ?> recognizer,
        final Object symbol,
        final int line,
        final int position,
        final String msg,
        final RecognitionException error
    ) {
        if (!Parser.class.isInstance(recognizer)) {
            throw new IllegalArgumentException(
                String.format(
                    "Illegal usage of %s, please use it to recognize only EO parser errors", this
                )
            );
        }
        final List<String> msgs = new ArrayList<>(0);
        if (error instanceof NoViableAltException) {
            final Token token = (Token) symbol;
            final Parser parser = (Parser) recognizer;
            final String rule = parser.getRuleInvocationStack().get(0);
            final String[] names = parser.getRuleNames();
            final String detailed;
            if (names[EoParser.RULE_objects].equals(rule)) {
                detailed = "Invalid object list declaration";
            } else if (names[EoParser.RULE_metas].equals(rule)) {
                detailed = "Invalid meta declaration";
            } else if (names[EoParser.RULE_program].equals(rule)) {
                detailed = "Invalid program declaration";
            } else if (names[EoParser.RULE_bound].equals(rule)) {
                detailed = "Invalid bound object declaration";
            } else if (names[EoParser.RULE_object].equals(rule)) {
                detailed = "Invalid object declaration";
            } else {
                detailed = "no viable alternative at input";
            }
            msgs.add(new MsgLocated(line, position, detailed).formatted());
            msgs.add(
                new MsgUnderlined(
                    this.lines.line(line),
                    position,
                    Math.max(token.getStopIndex() - token.getStartIndex(), 1)
                ).formatted()
            );
        } else if (error instanceof InputMismatchException) {
            final Token token = (Token) symbol;
            final Parser parser = (Parser) recognizer;
            final String rule = parser.getRuleInvocationStack().get(0);
            final String detailed;
            final String[] names = parser.getRuleNames();
            if (names[EoParser.RULE_program].equals(rule)) {
                detailed =
                    "We expected the program to end here but encountered something unexpected";
            } else if (names[EoParser.RULE_objects].equals(rule)) {
                detailed =
                    "We expected a list of objects here but encountered something unexpected";
            } else {
                detailed = msg;
            }
            msgs.add(new MsgLocated(line, position, detailed).formatted());
            msgs.add(
                new MsgUnderlined(
                    this.lines.line(line),
                    position,
                    Math.max(token.getStopIndex() - token.getStartIndex(), 1)
                ).formatted()
            );
        } else {
            msgs.add(new MsgLocated(line, position, msg).formatted());
            msgs.add(this.lines.line(line));
        }
        this.errors.add(new ParsingException(error, line, msgs));
    }

    @Override
    public Iterator<ParsingException> iterator() {
        return this.errors.iterator();
    }
}
