/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.cactoos.Text;
import org.cactoos.list.ListOf;

/**
 * Accumulates all parsing errors.
 *
 * @since 0.30.0
 */
final class GeneralErrors extends BaseErrorListener implements Iterable<ParsingException> {

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
     * @param lines The source in lines
     */
    GeneralErrors(final Text... lines) {
        this(new ListOf<>(lines));
    }

    /**
     * Ctor.
     * @param src The source in lines
     */
    GeneralErrors(final List<Text> src) {
        this(new ArrayList<>(0), new Lines(src));
    }

    /**
     * Ctor.
     * @param errors Errors accumulated
     * @param lines The source in lines
     */
    private GeneralErrors(final List<ParsingException> errors, final Lines lines) {
        this.errors = errors;
        this.lines = lines;
    }

    // @checkstyle ParameterNumberCheck (10 lines)
    @Override
    public void syntaxError(
        final Recognizer<?, ?> recognizer,
        final Object symbol,
        final int line,
        final int position,
        final String msg,
        final RecognitionException error
    ) {
        this.errors.add(
            new ParsingException(
                error,
                line,
                new MsgLocated(line, position, msg).formatted(),
                this.lines.line(line)
            )
        );
    }

    @Override
    public Iterator<ParsingException> iterator() {
        return this.errors.iterator();
    }
}
