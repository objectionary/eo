/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.regex.Pattern;

/**
 * A type written after a {@code /} — R-3.4.8 and R-3.10.10 of the spec.
 *
 * <p>An atom's return, a void's own type and every member of a callback
 * list share one grammar:</p>
 *
 * <pre>
 * type ::= one ( '|' one )?
 * one  ::= var | path ( '(' type ( ',' type )* ')' )?
 * path ::= NAME ( '.' NAME )* | 'Q' ( '.' NAME )+
 * var  ::= 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
 * </pre>
 *
 * <p>The brackets belong to the name right before them, a union holds
 * two types at most and counts on its own at every bracket level, and a
 * trailing {@code ?} covers the whole type. Whether that {@code ?} may
 * stand there at all is the caller's question, so a return and a callback
 * list reject it before this type ever sees it.</p>
 *
 * <p>A type is one word with no spaces. The caller cuts the word at the
 * first space, so this type also looks at the text that follows the word
 * and rejects a type that plainly goes on past the space, instead of
 * leaving the rest of the line to be reported as garbage.</p>
 *
 * <p>Every name in the tree passes the checks a single name passes, and
 * every {@code Q.} root becomes {@code Φ.} (R-9.3). The result keeps the
 * whole tree as one string, with its delimiters and type variables
 * verbatim. The walk keeps its own stack of open brackets instead of
 * recursing, so no depth of brackets can overflow the call stack.</p>
 *
 * @since 0.64
 */
final class TypeTree {

    /**
     * A {@code Q.} root at the head of a name inside the tree.
     */
    private static final Pattern ROOT = Pattern.compile("(^|[(),|])Q\\.");

    /**
     * A generic type variable, which takes no brackets.
     */
    private static final Pattern VARIABLE = Pattern.compile("[A-F]");

    /**
     * The glyphs that end a name inside the tree.
     */
    private static final String DELIMITERS = "(),|";

    /**
     * The message for brackets that follow no name.
     */
    private static final String FOLLOW = "brackets in a type must follow a name";

    /**
     * The message for a union that misses a side.
     */
    private static final String SIDES = "a union needs a type on each side of |";

    /**
     * The message for a comma that separates nothing.
     */
    private static final String COMMA =
        "a comma in a type must stand between two types inside brackets";

    /**
     * The message for a closing bracket with no opening one.
     */
    private static final String UNMATCHED = "unmatched closing bracket in a type";

    /**
     * The message for an opening bracket with no closing one.
     */
    private static final String UNCLOSED = "unclosed bracket in a type";

    /**
     * The type as written, with its trailing {@code ?} if any.
     */
    private final String word;

    /**
     * The source text right after the word.
     */
    private final String rest;

    /**
     * The source span, for error reporting.
     */
    private final Span span;

    /**
     * Source column of the first character of the word.
     */
    private final int pos;

    /**
     * Ctor.
     *
     * @param text The type as written
     * @param after The source text right after it
     * @param source The source span
     * @param column Source column of the type's first character
     */
    TypeTree(final String text, final String after, final Span source, final int column) {
        this.word = text;
        this.rest = after;
        this.span = source;
        this.pos = column;
    }

    /**
     * The type as XMIR keeps it, with every {@code Q.} root promoted to
     * {@code Φ.}.
     *
     * @return The promoted type
     */
    String asString() {
        final String body = this.body();
        this.checkSpaced(body);
        this.walk(body);
        return TypeTree.ROOT.matcher(body).replaceAll("$1Φ.")
            .concat(this.word.substring(body.length()));
    }

    private String body() {
        final String body;
        if (this.word.endsWith("?")) {
            body = this.word.substring(0, this.word.length() - 1);
        } else {
            body = this.word;
        }
        return body;
    }

    private void checkSpaced(final String body) {
        int idx = 0;
        while (idx < this.rest.length()
            && (this.rest.charAt(idx) == ' ' || this.rest.charAt(idx) == '\t')) {
            idx = idx + 1;
        }
        if (idx > 0 && (this.open(body)
            || idx < this.rest.length() && "()|,".indexOf(this.rest.charAt(idx)) >= 0)) {
            throw new ParseError(
                this.span.line(), this.pos + this.word.length(),
                "a type must not contain spaces"
            );
        }
    }

    private boolean open(final String body) {
        final long depth = body.chars().filter(glyph -> glyph == '(').count()
            - body.chars().filter(glyph -> glyph == ')').count();
        return depth > 0 || !body.isEmpty() && "(|,".indexOf(body.charAt(body.length() - 1)) >= 0;
    }

    private void walk(final String body) {
        final Deque<Integer> opens = new ArrayDeque<>(0);
        final Deque<Integer> bars = new ArrayDeque<>(1);
        bars.push(0);
        int idx = 0;
        boolean more = true;
        while (more) {
            final int end = this.name(body, idx, opens.isEmpty());
            final int next = this.closed(body, end, opens, bars);
            more = next < body.length();
            if (more) {
                this.checkFollowed(body, end, next);
                this.checkVariable(body.substring(idx, end), body.charAt(next), next);
                this.delimit(body.charAt(next), next, opens, bars);
            }
            idx = next + 1;
        }
        if (!opens.isEmpty()) {
            throw new ParseError(this.span.line(), this.pos + opens.peek(), TypeTree.UNCLOSED);
        }
    }

    private int name(final String body, final int from, final boolean top) {
        int end = from;
        while (end < body.length() && TypeTree.DELIMITERS.indexOf(body.charAt(end)) < 0) {
            end = end + 1;
        }
        if (end == from) {
            throw this.empty(body, from);
        }
        final String token = body.substring(from, end);
        final int mark = token.indexOf('?');
        if (mark >= 0 && top) {
            throw new ParseError(
                this.span.line(), this.pos + from + mark,
                "optional marker ? may stand only at the end of a void type"
            );
        }
        if (mark >= 0) {
            throw new ParseError(
                this.span.line(), this.pos + from + mark,
                "optional marker ? is not allowed inside brackets"
            );
        }
        Suffix.typeAtom(token, this.span, this.pos + from);
        return end;
    }

    private ParseError empty(final String body, final int idx) {
        final char next = this.glyph(body, idx);
        final char prev = this.glyph(body, idx - 1);
        final ParseError error;
        if (next == '(') {
            error = new ParseError(this.span.line(), this.pos + idx, TypeTree.FOLLOW);
        } else if (prev == '(' && next == ')') {
            error = new ParseError(
                this.span.line(), this.pos + idx,
                "brackets in a type must hold at least one type"
            );
        } else if (next == '|') {
            error = new ParseError(this.span.line(), this.pos + idx, TypeTree.SIDES);
        } else if (prev == '|') {
            error = new ParseError(this.span.line(), this.pos + idx - 1, TypeTree.SIDES);
        } else if (next == ',') {
            error = new ParseError(this.span.line(), this.pos + idx, TypeTree.COMMA);
        } else if (prev == ',') {
            error = new ParseError(this.span.line(), this.pos + idx - 1, TypeTree.COMMA);
        } else if (next == ')') {
            error = new ParseError(this.span.line(), this.pos + idx, TypeTree.UNMATCHED);
        } else {
            error = new ParseError(this.span.line(), this.pos + idx - 1, TypeTree.UNCLOSED);
        }
        return error;
    }

    private char glyph(final String body, final int idx) {
        final char glyph;
        if (idx >= 0 && idx < body.length()) {
            glyph = body.charAt(idx);
        } else {
            glyph = ' ';
        }
        return glyph;
    }

    private int closed(
        final String body, final int from, final Deque<Integer> opens, final Deque<Integer> bars
    ) {
        int idx = from;
        while (idx < body.length() && body.charAt(idx) == ')') {
            if (opens.isEmpty()) {
                throw new ParseError(this.span.line(), this.pos + idx, TypeTree.UNMATCHED);
            }
            opens.pop();
            bars.pop();
            idx = idx + 1;
        }
        return idx;
    }

    private void checkFollowed(final String body, final int end, final int next) {
        final char glyph = body.charAt(next);
        if (next > end && glyph == '(') {
            throw new ParseError(this.span.line(), this.pos + next, TypeTree.FOLLOW);
        }
        if (next > end && glyph != '|' && glyph != ',') {
            throw new ParseError(
                this.span.line(), this.pos + next,
                "unexpected name after a closing bracket in a type"
            );
        }
    }

    private void checkVariable(final String token, final char glyph, final int next) {
        if (glyph == '(' && TypeTree.VARIABLE.matcher(token).matches()) {
            throw new ParseError(
                this.span.line(), this.pos + next, "a type variable takes no brackets"
            );
        }
    }

    private void delimit(
        final char glyph, final int idx, final Deque<Integer> opens, final Deque<Integer> bars
    ) {
        if (glyph == '(') {
            opens.push(idx);
            bars.push(0);
        } else if (glyph == '|' && bars.peek() > 0) {
            throw new ParseError(
                this.span.line(), this.pos + idx, "a union holds at most two types"
            );
        } else if (glyph == '|') {
            bars.push(bars.pop() + 1);
        } else if (opens.isEmpty()) {
            throw new ParseError(this.span.line(), this.pos + idx, TypeTree.COMMA);
        } else {
            bars.pop();
            bars.push(0);
        }
    }
}
