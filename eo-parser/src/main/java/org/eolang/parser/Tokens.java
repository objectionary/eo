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
     * Characters that terminate a {@code NAME} token per §2.3, the dot
     * among them.
     */
    private static final String TERMINATORS = " \t,.|':;!?[]{}()";

    /**
     * Characters that break a parenthesised group into more than one
     * token, so their absence makes the group a single token.
     */
    private static final String BREAKERS = " ()[]";

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
            if (this.dottedDigit()) {
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
            Value.Kind.ROOT, String.valueOf(this.body.charAt(start)),
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
            final String mapped;
            if (this.current() == '@') {
                mapped = "φ";
            } else {
                mapped = "ρ";
            }
            value = new Value(
                Value.Kind.IDENTIFIER, mapped,
                this.span.indent() + this.cursor, this.cursor + 1
            );
            this.cursor = this.cursor + 1;
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
        return !this.atEnd() && (this.current() == '>' || this.plusArrow());
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

    /**
     * Whether the content of a paren group is a single token — it holds no
     * token separator ({@code ' '}, {@code '('}, {@code ')'}, {@code '['} or
     * {@code ']'}) outside of a quoted string literal. Characters inside
     * double quotes are ignored, since a string literal is always one atomic
     * token regardless of what it contains.
     * @param inside Content between the parentheses
     * @return True if the content is a single token
     */
    private static boolean singleToken(final String inside) {
        boolean single = true;
        int idx = 0;
        while (idx < inside.length()) {
            final char glyph = inside.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(inside, idx);
            } else if (Tokens.BREAKERS.indexOf(glyph) >= 0) {
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
        return Tokens.byteDigit(glyph) || glyph >= 'a' && glyph <= 'f';
    }

    /**
     * Whether a character is a valid BYTES hex digit. Per the grammar
     * (ANTLR's {@code BYTE : [0-9A-F][0-9A-F]}), BYTES accept only
     * uppercase hex — lowercase belongs to {@code NAME}.
     * @param glyph Character
     * @return True if 0-9 or A-F
     */
    private static boolean byteDigit(final char glyph) {
        return Tokens.digit(glyph) || glyph >= 'A' && glyph <= 'F';
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
     * Whether the root identifier at the index is glued to a NAME
     * character, as the legacy {@code QQ.io.stdout} head is. A root
     * ends at a NAME terminator, so anything else standing next to it
     * belongs to the same token.
     * @param body Body
     * @param idx Index of the root character
     * @return True if a NAME character follows the root
     */
    private static boolean glued(final String body, final int idx) {
        return idx + 1 < body.length()
            && !Tokens.terminates(body.charAt(idx + 1));
    }

    /**
     * Read the whole token starting at the index, up to the first
     * {@code NAME} terminator or the end of the body.
     * @param body Body
     * @param idx Index of the first character
     * @return The token, terminator excluded
     */
    private static String token(final String body, final int idx) {
        int end = idx;
        while (end < body.length() && !Tokens.terminates(body.charAt(end))) {
            end = end + 1;
        }
        return body.substring(idx, end);
    }

    /**
     * Whether the position is the start of an INT literal (digit or
     * signed digit).
     * @param body Body
     * @param idx Index
     * @return True if INT starts here
     */
    private static boolean digitStart(final String body, final int idx) {
        return Tokens.digitAt(body, idx) || Tokens.signedStart(body, idx);
    }

    /**
     * Whether a sign the first digit of an INT literal follows sits at
     * the given index.
     * @param body Body
     * @param idx Index
     * @return True if a signed INT starts here
     */
    private static boolean signedStart(final String body, final int idx) {
        return idx < body.length()
            && Tokens.sign(body.charAt(idx))
            && Tokens.digitAt(body, idx + 1);
    }

    /**
     * Whether a digit sits at the given index of the body, the index
     * being inside it at all.
     * @param body Body
     * @param idx Index
     * @return True if a digit is there
     */
    private static boolean digitAt(final String body, final int idx) {
        return idx < body.length() && Tokens.digit(body.charAt(idx));
    }

    /**
     * Whether a character is a decimal digit.
     * @param glyph Character
     * @return True if 0-9
     */
    private static boolean digit(final char glyph) {
        return glyph >= '0' && glyph <= '9';
    }

    /**
     * Whether a character is the sign of a number per R-3.2.5.
     * @param glyph Character
     * @return True if plus or minus
     */
    private static boolean sign(final char glyph) {
        return glyph == '+' || glyph == '-';
    }

    /**
     * Whether a character terminates a {@code NAME} token per §2.3.
     * @param glyph Character
     * @return True if it ends the token
     */
    private static boolean terminates(final char glyph) {
        return Tokens.TERMINATORS.indexOf(glyph) >= 0;
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
     * Advance the cursor just past the paren group that opens at
     * {@code start}, counting nested parens and treating a quoted string
     * as opaque, so a paren inside a literal never closes the group.
     * @param start Index of the opening paren
     */
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

    /**
     * Read one horizontal argument at the cursor — the value itself, the
     * method chain behind it when the value may carry one, and the
     * optional {@code :binding} and {@code !} suffixes (§3.12).
     * @return The argument
     */
    private Value readArg() {
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
        final boolean cnst;
        if (!this.atEnd() && this.current() == '!') {
            this.cursor = this.cursor + 1;
            cnst = true;
        } else {
            cnst = false;
        }
        return new Value(bare.kind(), bare.raw(), bare.pos(), this.cursor, tie, tail, cnst);
    }

    /**
     * Continue reading a FLOAT literal after the integer part — the
     * fractional digits and optional exponent.
     * @param start The starting cursor position (for error reporting)
     */
    private void readFloatTail(final int start) {
        this.cursor = this.cursor + 1;
        this.skipDigits();
        if (this.cursor < this.body.length()
            && (this.body.charAt(this.cursor) == 'e'
                || this.body.charAt(this.cursor) == 'E')) {
            this.readExponent(start);
        }
    }

    /**
     * Read the exponent of a FLOAT literal — the {@code e} or {@code E}
     * marker at the cursor, an optional sign, and at least one digit,
     * without which the literal is malformed (R-9.8.2).
     * @param start The starting cursor position (for error reporting)
     */
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

    /**
     * Advance the cursor past every decimal digit sitting at it.
     * @return How many digits were passed
     */
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

    /**
     * Read a value that no delimiter, digit, or root token introduces —
     * one of the reserved glyphs {@code *}, {@code T}, {@code %}, or a
     * NAME — rejecting a head that starts no value at all.
     * @param first The character at the cursor
     * @return Parsed value
     */
    private Value readGlyph(final char first) {
        if (first == '[') {
            throw new ParseError(
                this.span.line(), this.span.indent() + this.cursor,
                "horizontal formation not allowed as argument"
            );
        }
        final Value value;
        if (first == '*') {
            value = this.reserved(Value.Kind.STAR, "*");
        } else if (first == 'T') {
            value = this.reserved(Value.Kind.TERM, "T");
        } else if (first == '%') {
            value = this.reserved(Value.Kind.SELF, "%");
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
     * Make a value out of the one reserved glyph at the cursor and step
     * over it.
     * @param kind The kind that glyph denotes
     * @param raw The glyph itself
     * @return Parsed value
     */
    private Value reserved(final Value.Kind kind, final String raw) {
        this.cursor = this.cursor + 1;
        return new Value(
            kind, raw, this.span.indent() + this.cursor - 1, this.cursor
        );
    }

    /**
     * Scan the non-empty form of a BYTES literal at the cursor — one
     * {@code BB} pair, any number of {@code -BB} pairs after it, and an
     * optional trailing {@code -} per §3.13.1.
     * @param start Index the literal starts at, for error reporting
     * @return The literal text
     */
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
        if (this.cursor < this.body.length()
            && this.body.charAt(this.cursor) == '-') {
            this.cursor = this.cursor + 1;
        }
        return this.body.substring(start, this.cursor);
    }

    /**
     * Whether a {@code BB} pair of byte digits sits at the given index.
     * @param idx Index of the first digit
     * @return True if both characters are there and both are byte digits
     */
    private boolean bytePair(final int idx) {
        return idx + 1 < this.body.length()
            && Tokens.byteDigit(this.body.charAt(idx))
            && Tokens.byteDigit(this.body.charAt(idx + 1));
    }

    /**
     * Whether the cursor sits on the dot of a FLOAT fraction, that is,
     * on a dot a digit follows.
     * @return True if a fractional tail starts here
     */
    private boolean dottedDigit() {
        return this.cursor < this.body.length()
            && this.body.charAt(this.cursor) == '.'
            && Tokens.digitAt(this.body, this.cursor + 1);
    }

    /**
     * Whether a {@code +>} suffix marker starts at the cursor.
     * @return True if the marker is there
     */
    private boolean plusArrow() {
        return this.current() == '+'
            && this.cursor + 1 < this.body.length()
            && this.body.charAt(this.cursor + 1) == '>';
    }
}
