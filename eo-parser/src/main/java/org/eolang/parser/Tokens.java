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
 * @checkstyle CyclomaticComplexityCheck (820 lines)
 * @checkstyle BooleanExpressionComplexityCheck (820 lines)
 * @checkstyle NPathComplexityCheck (820 lines)
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.UnnecessaryLocalRule",
    "PMD.CognitiveComplexity",
    "PMD.NPathComplexity"
})
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
        } else if (first == '*') {
            value = new Value(
                Value.Kind.STAR, "*", this.span.indent() + this.cursor, this.cursor + 1
            );
            this.cursor = this.cursor + 1;
        } else if (first == '"') {
            value = this.readString();
        } else if (first == '(') {
            value = this.readGroup();
        } else if (Tokens.digitStart(this.body, this.cursor)) {
            value = this.readNumber();
        } else if (Tokens.rootStart(first)) {
            value = this.readRoot();
        } else if (first == 'T') {
            value = new Value(
                Value.Kind.TERM, "T", this.span.indent() + this.cursor, this.cursor + 1
            );
            this.cursor = this.cursor + 1;
        } else if (first == '%') {
            value = new Value(
                Value.Kind.SELF, "%", this.span.indent() + this.cursor, this.cursor + 1
            );
            this.cursor = this.cursor + 1;
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
            if (this.cursor + 1 >= this.body.length()
                || !Tokens.byteDigit(this.body.charAt(this.cursor))
                || !Tokens.byteDigit(this.body.charAt(this.cursor + 1))) {
                throw new ParseError(
                    this.span.line(), this.span.indent() + start,
                    "invalid bytes literal"
                );
            }
            this.cursor = this.cursor + 2;
            while (this.cursor + 2 < this.body.length()
                && this.body.charAt(this.cursor) == '-'
                && Tokens.byteDigit(this.body.charAt(this.cursor + 1))
                && Tokens.byteDigit(this.body.charAt(this.cursor + 2))) {
                this.cursor = this.cursor + 3;
            }
            if (this.cursor < this.body.length()
                && this.body.charAt(this.cursor) == '-') {
                this.cursor = this.cursor + 1;
            }
            raw = this.body.substring(start, this.cursor);
        }
        return new Value(
            Value.Kind.BYTES, raw,
            this.span.indent() + start, this.cursor
        );
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
        int depth = 1;
        this.cursor = this.cursor + 1;
        while (this.cursor < this.body.length() && depth > 0) {
            final char glyph = this.body.charAt(this.cursor);
            if (glyph == '"') {
                this.cursor = this.cursor + 1;
                while (this.cursor < this.body.length()
                    && this.body.charAt(this.cursor) != '"') {
                    if (this.body.charAt(this.cursor) == '\\'
                        && this.cursor + 1 < this.body.length()) {
                        this.cursor = this.cursor + 1;
                    }
                    this.cursor = this.cursor + 1;
                }
                if (this.cursor >= this.body.length()) {
                    throw new ParseError(
                        this.span.line(), this.span.indent() + start,
                        "unterminated string inside paren group"
                    );
                }
                this.cursor = this.cursor + 1;
            } else if (glyph == '(') {
                depth = depth + 1;
                this.cursor = this.cursor + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
                this.cursor = this.cursor + 1;
            } else {
                this.cursor = this.cursor + 1;
            }
        }
        if (depth != 0) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "unterminated paren group"
            );
        }
        final String inside = this.body.substring(start + 1, this.cursor - 1);
        if (Tokens.singleToken(inside)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                String.format(
                    "redundant parentheses around a single token — `(%s)` should be written as `%s`",
                    inside, inside
                )
            );
        }
        return new Value(
            Value.Kind.GROUP, this.body.substring(start, this.cursor),
            this.span.indent() + start, this.cursor
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
        if (raw.codePoints().anyMatch(cp -> cp == 0x1F335)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "cactus emoji is reserved for auto-names; not allowed in identifiers"
            );
        }
        this.cursor = idx;
        return new Value(
            Value.Kind.IDENTIFIER, raw,
            this.span.indent() + start, idx
        );
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
            if (this.cursor < this.body.length()
                && this.body.charAt(this.cursor) == '.'
                && this.cursor + 1 < this.body.length()
                && this.body.charAt(this.cursor + 1) >= '0'
                && this.body.charAt(this.cursor + 1) <= '9') {
                this.readFloatTail(start);
                value = new Value(
                    Value.Kind.FLOAT, this.body.substring(start, this.cursor),
                    this.span.indent() + start, this.cursor
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
        if (idx < this.body.length()
            && (this.body.charAt(idx) == '+' || this.body.charAt(idx) == '-')) {
            idx = idx + 1;
        }
        final int from = idx;
        while (idx < this.body.length()
            && this.body.charAt(idx) >= '0' && this.body.charAt(idx) <= '9') {
            idx = idx + 1;
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
            Value.Kind.INTEGER, this.body.substring(start, idx),
            this.span.indent() + start, idx
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
            Value.Kind.HEX, this.body.substring(start, this.cursor),
            this.span.indent() + start, this.cursor
        );
    }

    /**
     * Read a root identifier at the cursor — one of {@code Q}, {@code @},
     * {@code ^}, {@code $}. Advances past it.
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
        final char glyph = this.current();
        this.cursor = this.cursor + 1;
        return new Value(
            Value.Kind.ROOT, String.valueOf(glyph),
            this.span.indent() + start, this.cursor
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
                    this.span.indent() + start, this.cursor
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
            chain.add(new MethodChain(name.raw(), dot, name.end(), fragile));
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
            final int start = this.cursor;
            final String mapped;
            if (this.current() == '@') {
                mapped = "φ";
            } else {
                mapped = "ρ";
            }
            this.cursor = this.cursor + 1;
            value = new Value(
                Value.Kind.IDENTIFIER, mapped,
                this.span.indent() + start, this.cursor
            );
        } else {
            value = this.readName();
        }
        return value;
    }

    /**
     * Read zero or more space-separated horizontal arguments. Stops at
     * a suffix marker ({@code >}, {@code >>}, {@code +>}) or end of
     * body.
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
            if (this.atEnd()) {
                this.cursor = save;
                break;
            }
            if (this.suffixAhead()) {
                this.cursor = save;
                break;
            }
            final Value bare = this.readValue();
            final List<MethodChain> tail;
            if (Emissions.chainable(bare)) {
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
            args.add(
                new Value(bare.kind(), bare.raw(), bare.pos(), this.cursor, tie, tail)
            );
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
        if (!Tokens.validBinding(text)) {
            throw new ParseError(
                this.span.line(), this.span.indent() + start,
                "Invalid bound object declaration"
            );
        }
        return text;
    }

    /**
     * Whether the cursor is positioned at a name suffix marker.
     * @return True if a suffix starts here
     */
    boolean suffixAhead() {
        return !this.atEnd()
            && (this.current() == '>'
                || this.current() == '+'
                    && this.cursor + 1 < this.body.length()
                    && this.body.charAt(this.cursor + 1) == '>');
    }

    /**
     * The remainder of the body from the cursor onwards.
     * @return Tail substring
     */
    String tail() {
        return this.body.substring(this.cursor);
    }

    /**
     * The full source body the token stream operates on.
     * @return Body string
     */
    String body() {
        return this.body;
    }

    /**
     * Whether a paren group's content wraps a single, atomic token, which
     * makes the surrounding parentheses redundant.
     *
     * <p>The scan tracks quote state (toggled on an unescaped {@code "}) so
     * that spaces, parentheses and brackets living <em>inside</em> a string
     * literal don't fool it into thinking the group holds more than one token.
     * A quoted string is always a single token regardless of its contents, so
     * {@code ("hello")}, {@code ("hello world")} and {@code ("a(b)c")} are all
     * treated the same way — consistently redundant.</p>
     *
     * @param inside The text between the parentheses, exclusive
     * @return True if the parentheses wrap exactly one token
     */
    private static boolean singleToken(final String inside) {
        boolean single = !inside.isEmpty();
        boolean quoted = false;
        int idx = 0;
        while (idx < inside.length()) {
            final char glyph = inside.charAt(idx);
            if (quoted) {
                if (glyph == '\\' && idx + 1 < inside.length()) {
                    idx = idx + 1;
                } else if (glyph == '"') {
                    quoted = false;
                }
            } else if (glyph == '"') {
                quoted = true;
            } else if (glyph == ' '
                || glyph == '('
                || glyph == ')'
                || glyph == '['
                || glyph == ']') {
                single = false;
                break;
            }
            idx = idx + 1;
        }
        return single;
    }

    /**
     * Whether the fragile {@code ?.} operator begins at {@code idx} —
     * a {@code ?} immediately followed by a {@code .} (R-3.5).
     * @param body Body
     * @param idx Index
     * @return True if {@code ?.} starts here
     */
    private static boolean fragileAhead(final String body, final int idx) {
        return idx + 1 < body.length()
            && body.charAt(idx) == '?' && body.charAt(idx + 1) == '.';
    }

    /**
     * Whether the position starts a BYTES literal — either {@code --}
     * or a {@code HH-} pattern.
     * @param body Body
     * @param idx Index
     * @return True if BYTES starts here
     */
    private static boolean bytesStart(final String body, final int idx) {
        return idx + 1 < body.length()
            && body.charAt(idx) == '-'
            && body.charAt(idx + 1) == '-'
            || Tokens.byteChunk(body, idx);
    }

    /**
     * Whether the position holds exactly a {@code HH-} byte chunk.
     * @param body Body
     * @param idx Index
     * @return True if {@code HH-} appears here
     */
    private static boolean byteChunk(final String body, final int idx) {
        return idx + 2 < body.length()
            && Tokens.byteDigit(body.charAt(idx))
            && Tokens.byteDigit(body.charAt(idx + 1))
            && body.charAt(idx + 2) == '-';
    }

    /**
     * Whether a character is a valid hex digit (case-insensitive per
     * R-9.8.3) for use inside a {@code 0x...} HEX literal.
     * @param glyph Character
     * @return True if 0-9, A-F, or a-f
     */
    private static boolean hexDigit(final char glyph) {
        return glyph >= '0' && glyph <= '9'
            || glyph >= 'A' && glyph <= 'F'
            || glyph >= 'a' && glyph <= 'f';
    }

    /**
     * Whether a character is a valid BYTES hex digit. Per the grammar
     * (ANTLR's {@code BYTE : [0-9A-F][0-9A-F]}), BYTES accept only
     * uppercase hex — lowercase belongs to {@code NAME}.
     * @param glyph Character
     * @return True if 0-9 or A-F
     */
    private static boolean byteDigit(final char glyph) {
        return glyph >= '0' && glyph <= '9' || glyph >= 'A' && glyph <= 'F';
    }

    /**
     * Whether the character is a root identifier source token
     * ({@code Q}, {@code @}, {@code ^}, {@code $}).
     * @param glyph Character
     * @return True if it is a root token
     */
    private static boolean rootStart(final char glyph) {
        return glyph == 'Q' || glyph == '@' || glyph == '^' || glyph == '$';
    }

    /**
     * Whether the position is the start of an INT literal (digit or
     * signed digit).
     * @param body Body
     * @param idx Index
     * @return True if INT starts here
     */
    private static boolean digitStart(final String body, final int idx) {
        final boolean result;
        if (idx >= body.length()) {
            result = false;
        } else {
            final char glyph = body.charAt(idx);
            if (glyph >= '0' && glyph <= '9') {
                result = true;
            } else if ((glyph == '+' || glyph == '-')
                && idx + 1 < body.length()
                && body.charAt(idx + 1) >= '0' && body.charAt(idx + 1) <= '9') {
                result = true;
            } else {
                result = false;
            }
        }
        return result;
    }

    /**
     * Whether a character terminates a {@code NAME} token per §2.3.
     * @param glyph Character
     * @return True if it ends the token
     */
    private static boolean terminates(final char glyph) {
        return glyph == ' '
            || glyph == '\t'
            || glyph == ','
            || glyph == '.'
            || glyph == '|'
            || glyph == '\''
            || glyph == ':'
            || glyph == ';'
            || glyph == '!'
            || glyph == '?'
            || glyph == ']'
            || glyph == '['
            || glyph == '}'
            || glyph == '{'
            || glyph == ')'
            || glyph == '(';
    }

    /**
     * Whether {@code text} is a legal inline-binding label — a NAME
     * (lowercase letter then NAME chars) or a non-negative integer
     * literal (digits only, no leading zero unless the value itself is
     * {@code 0}) per §3.12.
     * @param text The binding text
     * @return True if legal
     */
    private static boolean validBinding(final String text) {
        final boolean valid;
        if (text.isEmpty()) {
            valid = false;
        } else if (text.charAt(0) >= 'a' && text.charAt(0) <= 'z') {
            valid = true;
        } else if (text.chars().allMatch(ch -> ch >= '0' && ch <= '9')) {
            valid = text.charAt(0) != '0' || "0".equals(text);
        } else {
            valid = false;
        }
        return valid;
    }

    /**
     * Continue reading a FLOAT literal after the integer part — the
     * fractional digits and optional exponent.
     * @param start The starting cursor position (for error reporting)
     * @checkstyle CyclomaticComplexityCheck (30 lines)
     */
    private void readFloatTail(final int start) {
        this.cursor = this.cursor + 1;
        while (this.cursor < this.body.length()
            && this.body.charAt(this.cursor) >= '0'
            && this.body.charAt(this.cursor) <= '9') {
            this.cursor = this.cursor + 1;
        }
        if (this.cursor < this.body.length()
            && (this.body.charAt(this.cursor) == 'e'
                || this.body.charAt(this.cursor) == 'E')) {
            this.cursor = this.cursor + 1;
            if (this.cursor < this.body.length()
                && (this.body.charAt(this.cursor) == '+'
                    || this.body.charAt(this.cursor) == '-')) {
                this.cursor = this.cursor + 1;
            }
            final int exp = this.cursor;
            while (this.cursor < this.body.length()
                && this.body.charAt(this.cursor) >= '0'
                && this.body.charAt(this.cursor) <= '9') {
                this.cursor = this.cursor + 1;
            }
            if (this.cursor == exp) {
                throw new ParseError(
                    this.span.line(), this.span.indent() + start,
                    "invalid signed-number literal"
                );
            }
        }
    }
}
