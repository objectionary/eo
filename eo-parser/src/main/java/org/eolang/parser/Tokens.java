/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A stream-style token reader over one source-line body.
 *
 * <p>Owns a mutable cursor that advances through the body as each
 * {@code read*} method consumes tokens. Used by line shapes
 * ({@link LnApplication}, {@link LnMethod}, {@link LnReversed},
 * …) to parse identifiers, INT literals, star tuples, method-dispatch
 * chains, and argument lists with a uniform API — avoiding the same
 * tokenisation logic being repeated in every line class.</p>
 *
 * <p>The cursor is mutable by design (the
 * parser-pragmatism rule weighs allocation cost over immutability).
 * Errors thrown are {@link ParseError} with the canonical message
 * texts from §9.9.</p>
 *
 * @since 0.1
 */
final class Tokens {

    /**
     * The line body being scanned.
     */
    private final String body;

    /**
     * The containing source span, used for error positions.
     */
    private final Span span;

    /**
     * Current cursor position (0-indexed into {@link #body}).
     */
    private int cursor;

    /**
     * Ctor.
     * @param text Line body
     * @param source Source span
     */
    Tokens(final String text, final Span source) {
        this.body = text;
        this.span = source;
        this.cursor = 0;
    }

    /**
     * Current cursor position.
     * @return Cursor
     */
    int cursor() {
        return this.cursor;
    }

    /**
     * Move the cursor.
     * @param idx New position
     */
    void seek(final int idx) {
        this.cursor = idx;
    }

    /**
     * Whether the cursor is past the end of the body.
     * @return End flag
     */
    boolean atEnd() {
        return this.cursor >= this.body.length();
    }

    /**
     * Peek the current character (must not be {@link #atEnd()}).
     * @return Current character
     */
    char current() {
        return this.body.charAt(this.cursor);
    }

    /**
     * Read one value at the cursor — identifier, INT, FLOAT, STRING,
     * STAR, or ROOT. Advances the cursor past the value.
     * @return Parsed value
     */
    Value readValue() {
        if (this.atEnd()) {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "expected value"
            );
        }
        final Value value;
        final char first = this.current();
        if (Tokens.bytesStart(this.body, this.cursor)) {
            value = this.readBytes();
        } else if (first == '"') {
            value = this.readString();
        } else if (first == '(') {
            value = this.readGroup();
        } else if (Tokens.digitStart(this.body, this.cursor)) {
            value = this.readNumber();
        } else if (Tokens.rootStart(first)) {
            value = this.readRoot();
        } else {
            value = this.readGlyph(first);
        }
        return value;
    }

    /**
     * Read a BYTES literal at the cursor — {@code --}, {@code BB-}, or
     * {@code BB-BB(-BB)*} per §3.13.1. Multi-line continuation
     * (R-3.13.2/R-3.13.3) is not yet supported here.
     * @return BYTES value
     */
    Value readBytes() {
        final int start = this.cursor;
        final String raw;
        if (this.cursor + 1 < this.body.length()
            && this.body.charAt(this.cursor) == '-'
            && this.body.charAt(this.cursor + 1) == '-') {
            this.cursor = this.cursor + 2;
            raw = "--";
        } else {
            raw = this.readPairs(start);
        }
        return new Value(Value.Kind.BYTES, raw, this.span.indent() + start);
    }

    /**
     * Read a paren group {@code (expr)} at the cursor. Scans to the
     * matching {@code )} with nested-paren and string awareness.
     * Returns a {@link Value.Kind#GROUP} carrying the literal bracketed
     * text (parens included). Used both as a head and as an arg.
     * @return GROUP value
     */
    Value readGroup() {
        if (this.atEnd() || this.current() != '(') {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "expected `(`"
            );
        }
        final int start = this.cursor;
        this.skipGroup(start);
        final String inside = this.body.substring(start + 1, this.cursor - 1);
        if (!inside.isEmpty() && Tokens.singleToken(inside)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                String.format(
                    "redundant parentheses around a single token — `(%s)` should be written as `%s`",
                    inside, inside
                )
            );
        }
        return new Value(
            Value.Kind.GROUP, this.body.substring(start, this.cursor), this.span.indent() + start
        );
    }

    /**
     * Read a {@code NAME} identifier starting at the cursor. Advances
     * past the name.
     * @return Identifier value
     */
    Value readName() {
        final int start = this.cursor;
        if (this.atEnd()) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "expected identifier"
            );
        }
        final char first = this.current();
        if (first < 'a' || first > 'z') {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "expected identifier"
            );
        }
        int idx = start + 1;
        while (idx < this.body.length() && !Tokens.terminates(this.body.charAt(idx))) {
            idx = idx + 1;
        }
        final String raw = this.body.substring(start, idx);
        if (Tokens.cactus(raw)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "cactus emoji is reserved for auto-names; not allowed in identifiers"
            );
        }
        final int control = new Scrubbed(raw).found();
        if (control >= 0) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start + control,
                "control character is not allowed in an identifier"
            );
        }
        this.cursor = idx;
        return new Value(Value.Kind.IDENTIFIER, raw, this.span.indent() + start);
    }

    /**
     * Read an INT or FLOAT literal at the cursor — optional sign, then
     * digits, optionally followed by {@code .digits} and exponent for
     * a FLOAT (R-9.8.1 / R-9.8.2). The choice between INT and FLOAT is
     * decided by lookahead: a dot followed by a digit continues as
     * FLOAT; otherwise the digits stop and the dot belongs to a
     * subsequent chain link.
     * @return INT or FLOAT value
     */
    Value readNumber() {
        final int start = this.cursor;
        final Value value;
        if (this.cursor + 1 < this.body.length()
            && this.body.charAt(this.cursor) == '0'
            && this.body.charAt(this.cursor + 1) == 'x') {
            value = this.readHex();
        } else {
            final Value integer = this.readInt();
            if (this.dottedDigit()) {
                this.readFloatTail(start);
                value = new Value(
                    Value.Kind.FLOAT, this.body.substring(start, this.cursor),
                    this.span.indent() + start
                );
            } else {
                value = integer;
            }
        }
        return value;
    }

    /**
     * Read an INT literal at the cursor — optional sign then digits,
     * per R-9.8.1 (no leading zeros). Advances past it.
     * @return INT value
     */
    Value readInt() {
        final int start = this.cursor;
        int idx = start;
        final boolean sign = idx < this.body.length()
            && (this.body.charAt(idx) == '+' || this.body.charAt(idx) == '-');
        if (sign) {
            idx = idx + 1;
        }
        final int from = idx;
        while (Tokens.digitAt(this.body, idx)) {
            idx = idx + 1;
        }
        if (sign && Tokens.letterAt(this.body, idx)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "invalid signed-number literal"
            );
        }
        if (idx == from) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "invalid signed-number literal"
            );
        }
        final String digits = this.body.substring(from, idx);
        if (digits.length() >= 2 && digits.charAt(0) == '0') {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "integer literal must not have leading zeros"
            );
        }
        this.cursor = idx;
        return new Value(
            Value.Kind.INTEGER, this.body.substring(start, idx), this.span.indent() + start
        );
    }

    /**
     * Read a HEX literal at the cursor ({@code 0x} followed by hex
     * digits, per R-9.8.3). Advances past it.
     * @return HEX value
     */
    Value readHex() {
        final int start = this.cursor;
        if (this.cursor + 1 >= this.body.length()
            || this.body.charAt(this.cursor) != '0'
            || this.body.charAt(this.cursor + 1) != 'x') {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "expected hexadecimal literal"
            );
        }
        this.cursor = this.cursor + 2;
        final int from = this.cursor;
        while (this.cursor < this.body.length()
            && Tokens.hexDigit(this.body.charAt(this.cursor))) {
            this.cursor = this.cursor + 1;
        }
        if (this.cursor == from) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "hexadecimal literal requires at least one digit"
            );
        }
        return new Value(
            Value.Kind.HEX, this.body.substring(start, this.cursor), this.span.indent() + start
        );
    }

    /**
     * Read a root identifier at the cursor — one of {@code Q}, {@code @},
     * {@code ^}, {@code $}. Advances past it. A root glued to a letter
     * or a digit — the legacy {@code QQ} above all — is rejected here,
     * so that the offending token names itself instead of surfacing as
     * trailing garbage after a one-character expression.
     * @return Root value
     */
    Value readRoot() {
        if (this.atEnd() || !Tokens.rootStart(this.current())) {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "expected root identifier"
            );
        }
        final int start = this.cursor;
        if (Tokens.glued(this.body, start)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                String.format(
                    "%s is not a valid object name, root %s must be followed by a dot",
                    Tokens.token(this.body, start), this.body.charAt(start)
                )
            );
        }
        this.cursor = this.cursor + 1;
        return new Value(
            Value.Kind.ROOT, String.valueOf(this.body.charAt(start)), this.span.indent() + start
        );
    }

    /**
     * Read a {@code STRING} literal at the cursor — {@code "..."} with
     * escape sequences (R-9.7). The returned {@link Value#raw()}
     * preserves the literal text <em>including the surrounding double
     * quotes</em>, so the emitter can strip and unescape consistently.
     * @return STRING value
     */
    Value readString() {
        if (this.atEnd() || this.current() != '"') {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "expected string literal"
            );
        }
        final int start = this.cursor;
        this.cursor = this.cursor + 1;
        while (this.cursor < this.body.length()) {
            final char glyph = this.body.charAt(this.cursor);
            if (glyph == '\n' || glyph == '\r') {
                throw new ParseError(
                    this.span.line(), this.span.indent() + this.cursor,
                    "unterminated string literal"
                );
            }
            if (glyph == '\\') {
                if (this.cursor + 1 >= this.body.length()) {
                    throw new ParseError(
                        this.span.line(), this.span.indent() + this.cursor,
                        "unterminated string literal"
                    );
                }
                this.cursor = this.cursor + 2;
            } else if (glyph == '"') {
                this.cursor = this.cursor + 1;
                return new Value(
                    Value.Kind.STRING, this.body.substring(start, this.cursor),
                    this.span.indent() + start
                );
            } else {
                this.cursor = this.cursor + 1;
            }
        }
        throw new ParseError(
            this.span.line(), this.span.indent() + start,
            "unterminated string literal"
        );
    }

    /**
     * Read zero or more {@code .NAME} chain links following the
     * cursor.
     * @return Chain links in source order
     */
    List<MethodChain> readChain() {
        final List<MethodChain> chain = new ArrayList<>(0);
        while (!this.atEnd() && this.dispatchAhead()) {
            final boolean fragile = Tokens.fragileAhead(this.body, this.cursor);
            int dot = this.span.indent() + this.cursor;
            if (fragile) {
                dot = dot + 1;
            }
            this.consumeDispatch();
            final Value name = this.readMethodName();
            chain.add(new MethodChain(name.raw(), dot, fragile));
        }
        return chain;
    }

    /**
     * Whether a dispatch operator begins at the cursor — a plain
     * {@code .} or the fragile {@code ?.} (R-3.5). The {@code ?} alone,
     * not followed by {@code .}, is not a dispatch.
     * @return True if a {@code .} or {@code ?.} dispatch starts here
     */
    boolean dispatchAhead() {
        return this.current() == '.' || Tokens.fragileAhead(this.body, this.cursor);
    }

    /**
     * Consume a dispatch operator at the cursor — a plain {@code .} or
     * the fragile {@code ?.} (R-3.5) — and report which it was. The
     * cursor must sit on a {@link #dispatchAhead()} position.
     * @return True if the consumed operator was the fragile {@code ?.}
     */
    boolean consumeDispatch() {
        final boolean fragile = Tokens.fragileAhead(this.body, this.cursor);
        if (fragile) {
            this.cursor = this.cursor + 1;
        }
        this.cursor = this.cursor + 1;
        return fragile;
    }

    /**
     * Read a method-dispatch name — either a regular {@code NAME}, or
     * the {@code @}/{@code ^} root tokens which map to {@code φ} /
     * {@code ρ} per R-3.5.2 / R-9.3.
     * @return Method name value
     */
    Value readMethodName() {
        final Value value;
        if (!this.atEnd() && (this.current() == '@' || this.current() == '^')) {
            final String mapped;
            if (this.current() == '@') {
                mapped = "φ";
            } else {
                mapped = "ρ";
            }
            value = new Value(Value.Kind.IDENTIFIER, mapped, this.span.indent() + this.cursor);
            this.cursor = this.cursor + 1;
        } else {
            value = this.readName();
        }
        return value;
    }

    /**
     * Read zero or more space-separated horizontal arguments. Stops at
     * a suffix marker ({@code >}, {@code >>}, {@code +>}, {@code ->}) or
     * end of body.
     * @return Arguments in source order
     */
    List<Value> readArgs() {
        final List<Value> args = new ArrayList<>(0);
        while (!this.atEnd()) {
            if (this.current() != ' ') {
                break;
            }
            final int save = this.cursor;
            this.cursor = this.cursor + 1;
            if (this.atEnd() || this.suffixAhead()) {
                this.cursor = save;
                break;
            }
            args.add(this.readArg());
        }
        return args;
    }

    /**
     * Read an inline-binding label or numeric slot — characters up to
     * the next whitespace or NAME terminator. Used for {@code :label}
     * and {@code :N} per §3.12.
     * @return Binding text
     */
    String readBinding() {
        final int start = this.cursor;
        while (!this.atEnd() && !Tokens.terminates(this.current())) {
            this.cursor = this.cursor + 1;
        }
        if (this.cursor == start) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "expected binding label after `:`"
            );
        }
        final String text = this.body.substring(start, this.cursor);
        Tokens.checkBinding(text, this.span, this.span.indent() + start);
        return text;
    }

    /**
     * Check an inline-binding label or numeric slot against §3.12 and
     * reject anything that is neither a NAME-initial label nor a plain
     * slot number. Shared by {@link Tokens#readBinding()} and the outer
     * binding of a vertical formation, so both spell the same grammar.
     * @param text Binding text, without the leading {@code :}
     * @param span Source span
     * @param pos Source column of the label (for errors)
     */
    static void checkBinding(final String text, final Span span, final int pos) {
        if (Tokens.cactus(text)) {
            throw new ParseError(
                span.line(), pos,
                "cactus emoji is reserved for auto-names; not allowed in identifiers"
            );
        }
        if (!Tokens.validBinding(text)) {
            throw new ParseError(
                span.line(), pos,
                String.format(
                    "binding label \"%s\" must be a name or a slot number",
                    text
                )
            );
        }
    }

    /**
     * Whether the cursor is positioned at a name suffix marker.
     * @return True if a suffix starts here
     */
    boolean suffixAhead() {
        return !this.atEnd()
            && (this.current() == '>' || this.plusArrow() || this.minusArrow());
    }

    /**
     * The remainder of the body from the cursor onwards.
     * @return Tail substring
     */
    String tail() {
        return this.body.substring(this.cursor);
    }

    /**
     * Reject whatever the cursor has left ahead of it. A construct that
     * owns the whole body it was handed — a parenthesised expression, for
     * one — says so once its reader is done, and the content the reader
     * could not place is reported rather than dropped.
     * @param message What to say about the leftovers
     */
    void checkEnd(final String message) {
        final String rest = this.tail().stripLeading();
        if (!rest.isEmpty()) {
            throw new ParseError(
                this.span.line(),
                this.span.indent() + this.body.length() - rest.length(),
                message
            );
        }
    }

    /**
     * The full source body the token stream operates on.
     * @return Body string
     */
    String body() {
        return this.body;
    }

    /**
     * The index of the quote closing the string literal that opens at
     * {@code start}, or the length of the text when the literal is never
     * closed. A backslash escapes the glyph behind it, so an escaped quote
     * leaves the literal open. Every scanner in this package walks a
     * literal through here, so quoted text stays opaque the same way
     * whether it is met by the lexer or by a top-level marker search.
     * @param text Text being scanned
     * @param start Index of the opening quote
     * @return Index of the closing quote
     */
    static int closingQuote(final String text, final int start) {
        int idx = start + 1;
        while (idx < text.length() && text.charAt(idx) != '"') {
            if (text.charAt(idx) == '\\' && idx + 1 < text.length()) {
                idx = idx + 1;
            }
            idx = idx + 1;
        }
        return idx;
    }

    private static boolean singleToken(final String inside) {
        boolean single = true;
        int idx = 0;
        while (idx < inside.length()) {
            final char glyph = inside.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(inside, idx);
            } else if (" ()[]".indexOf(glyph) >= 0) {
                single = false;
                break;
            }
            idx = idx + 1;
        }
        return single;
    }

    private static boolean fragileAhead(final String body, final int idx) {
        return idx + 1 < body.length() && body.charAt(idx) == '?' && body.charAt(idx + 1) == '.';
    }

    private static boolean bytesStart(final String body, final int idx) {
        return idx + 1 < body.length()
            && body.charAt(idx) == '-'
            && body.charAt(idx + 1) == '-'
            || Tokens.byteChunk(body, idx);
    }

    private static boolean byteChunk(final String body, final int idx) {
        return idx + 2 < body.length() && Tokens.byteDigit(body.charAt(idx))
            && Tokens.byteDigit(body.charAt(idx + 1)) && body.charAt(idx + 2) == '-';
    }

    private static boolean hexDigit(final char glyph) {
        return Tokens.byteDigit(glyph) || glyph >= 'a' && glyph <= 'f';
    }

    private static boolean letterAt(final String body, final int idx) {
        return idx < body.length()
            && body.charAt(idx) < 128
            && Character.isLetter(body.charAt(idx));
    }

    private static boolean byteDigit(final char glyph) {
        return Tokens.digit(glyph) || glyph >= 'A' && glyph <= 'F';
    }

    private static boolean rootStart(final char glyph) {
        return glyph == 'Q' || glyph == '@' || glyph == '^' || glyph == '$';
    }

    private static boolean glued(final String body, final int idx) {
        return idx + 1 < body.length() && !Tokens.terminates(body.charAt(idx + 1));
    }

    private static String token(final String body, final int idx) {
        int end = idx;
        while (end < body.length() && !Tokens.terminates(body.charAt(end))) {
            end = end + 1;
        }
        return body.substring(idx, end);
    }

    private static boolean digitStart(final String body, final int idx) {
        return Tokens.digitAt(body, idx) || Tokens.signedStart(body, idx);
    }

    private static boolean signedStart(final String body, final int idx) {
        return idx < body.length()
            && Tokens.sign(body.charAt(idx)) && Tokens.digitAt(body, idx + 1);
    }

    private static boolean digitAt(final String body, final int idx) {
        return idx < body.length() && Tokens.digit(body.charAt(idx));
    }

    private static boolean digit(final char glyph) {
        return glyph >= '0' && glyph <= '9';
    }

    private static boolean sign(final char glyph) {
        return glyph == '+' || glyph == '-';
    }

    private static boolean terminates(final char glyph) {
        return " \t,.|':;!?[]{}()".indexOf(glyph) >= 0;
    }

    private static boolean cactus(final String text) {
        return text.codePoints().anyMatch(cp -> cp == 0x1F335);
    }

    private static boolean validBinding(final String text) {
        final boolean valid;
        if (text.isEmpty()) {
            valid = false;
        } else if ("^".equals(text)
            || text.charAt(0) >= 'a' && text.charAt(0) <= 'z') {
            valid = true;
        } else if (text.chars().allMatch(ch -> ch >= '0' && ch <= '9')) {
            valid = text.charAt(0) != '0' || "0".equals(text);
        } else {
            valid = false;
        }
        return valid;
    }

    private void skipGroup(final int start) {
        int depth = 1;
        this.cursor = this.cursor + 1;
        while (this.cursor < this.body.length() && depth > 0) {
            final char glyph = this.body.charAt(this.cursor);
            if (glyph == '"') {
                this.cursor = Tokens.closingQuote(this.body, this.cursor);
                if (this.cursor >= this.body.length()) {
                    throw new ParseError(
                        this.span.line(), this.span.indent() + start,
                        "unterminated string inside paren group"
                    );
                }
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            }
            this.cursor = this.cursor + 1;
        }
        if (depth != 0) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "unterminated paren group"
            );
        }
    }

    private Value readArg() {
        final Value bare = this.readValue();
        final List<MethodChain> tail;
        if (bare.chainable()) {
            tail = this.readChain();
        } else {
            tail = Collections.emptyList();
        }
        final String tie;
        if (!this.atEnd() && this.current() == ':') {
            this.cursor = this.cursor + 1;
            tie = this.readBinding();
        } else {
            tie = null;
        }
        final boolean cnst;
        if (!this.atEnd() && this.current() == '!') {
            this.cursor = this.cursor + 1;
            cnst = true;
        } else {
            cnst = false;
        }
        return new Value(bare.kind(), bare.raw(), bare.pos(), tie, tail, cnst);
    }

    private void readFloatTail(final int start) {
        this.cursor = this.cursor + 1;
        this.skipDigits();
        if (this.cursor < this.body.length()
            && (this.body.charAt(this.cursor) == 'e'
                || this.body.charAt(this.cursor) == 'E')) {
            this.readExponent(start);
        }
    }

    private void readExponent(final int start) {
        this.cursor = this.cursor + 1;
        if (this.cursor < this.body.length()
            && (this.body.charAt(this.cursor) == '+'
                || this.body.charAt(this.cursor) == '-')) {
            this.cursor = this.cursor + 1;
        }
        if (this.skipDigits() == 0) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "invalid signed-number literal"
            );
        }
    }

    private int skipDigits() {
        int count = 0;
        while (this.cursor < this.body.length()
            && this.body.charAt(this.cursor) >= '0'
            && this.body.charAt(this.cursor) <= '9') {
            this.cursor = this.cursor + 1;
            count = count + 1;
        }
        return count;
    }

    private Value readGlyph(final char first) {
        if (first == '[') {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "horizontal formation not allowed as argument"
            );
        }
        if (this.oddHexRun()) {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "invalid bytes literal"
            );
        }
        final Value value;
        if (first == '*') {
            value = this.reserved(Value.Kind.STAR, "*");
        } else if (first == 'T') {
            value = this.reserved(Value.Kind.TERM, "T");
        } else if (first == 'I') {
            value = this.reserved(Value.Kind.IDENTITY, "I");
        } else if (first >= 'a' && first <= 'z') {
            value = this.readName();
        } else {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "expected value"
            );
        }
        return value;
    }

    private Value reserved(final Value.Kind kind, final String raw) {
        this.cursor = this.cursor + 1;
        return new Value(kind, raw, this.span.indent() + this.cursor - 1);
    }

    private String readPairs(final int start) {
        if (!this.bytePair(this.cursor)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "invalid bytes literal"
            );
        }
        this.cursor = this.cursor + 2;
        while (this.cursor < this.body.length()
            && this.body.charAt(this.cursor) == '-'
            && this.bytePair(this.cursor + 1)) {
            this.cursor = this.cursor + 3;
        }
        if (this.cursor < this.body.length() && this.body.charAt(this.cursor) == '-') {
            if (this.cursor - start > 2) {
                throw new ParseError(
                    this.span.line(), this.span.indent() + this.cursor,
                    "bytes literal ends with a dangling continuation dash"
                );
            }
            this.cursor = this.cursor + 1;
        }
        return this.body.substring(start, this.cursor);
    }

    private boolean oddHexRun() {
        int idx = this.cursor;
        while (idx < this.body.length() && Tokens.byteDigit(this.body.charAt(idx))) {
            idx = idx + 1;
        }
        return idx > this.cursor
            && idx < this.body.length()
            && this.body.charAt(idx) == '-';
    }

    private boolean bytePair(final int idx) {
        return idx + 1 < this.body.length()
            && Tokens.byteDigit(this.body.charAt(idx))
            && Tokens.byteDigit(this.body.charAt(idx + 1));
    }

    private boolean dottedDigit() {
        return this.cursor < this.body.length()
            && this.body.charAt(this.cursor) == '.'
            && Tokens.digitAt(this.body, this.cursor + 1);
    }

    private boolean plusArrow() {
        return this.current() == '+'
            && this.cursor + 1 < this.body.length()
            && this.body.charAt(this.cursor + 1) == '>';
    }

    private boolean minusArrow() {
        return this.current() == '-'
            && this.cursor + 1 < this.body.length()
            && this.body.charAt(this.cursor + 1) == '>';
    }
}
