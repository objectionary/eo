/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Shared {@link Value}-to-XMIR rendering helpers.
 *
 * <p>Different line shapes ({@link LnApplication}, {@link LnMethod},
 * {@link LnReversed}, {@link LnCompactTuple}, {@link LnOnlyPhi},
 * …) all need to render parsed {@link Value}s and full expressions
 * into XMIR. This class centralises the recipes so every line emits
 * literals and chains in exactly the same way (§9.0.3 / §9.4 /
 * §9.4.2).</p>
 *
 * <p>A reversed dispatch emitted here keeps the head text as the
 * {@code .}-prefixed base, except a root glyph ({@code ^}, {@code @},
 * {@code $}), which maps to {@code ρ}/{@code φ}/{@code ξ} the way
 * {@link LnReversed#readHead} does.</p>
 *
 * @since 0.1
 * @todo #8244:30min Reject a receiverless reversed dispatch used as the
 *  phi of a parenthesised inline-phi formation, e.g.
 *  {@code bar (if. > [x]) > z}, the same way LnOnlyPhi now does for the
 *  vertical-body shape (see #8244).
 */
final class Emissions {

    /**
     * What the parser says when the identity object is applied to
     * arguments (R-3.16.1). Shared by every position an application may
     * be written in — a line head ({@link LnApplication}), a paren
     * group, an inline φ, and the φ of an only-phi line — so the writer
     * gets the same instruction wherever the glyph carries arguments.
     */
    static final String NO_IDENTITY_ARGS = String.join(
        " ",
        "the identity object takes no horizontal arguments;",
        "put the argument on a deeper-indent line"
    );

    /**
     * The void the identity object {@code I} binds and decorates.
     */
    private static final String IDENTITY = "x";

    /**
     * A valid void parameter name, other than the {@code @} and {@code ^}
     * special forms — §4.5. Shared by every producer of a void parameter
     * list ({@link LnFormation}, {@link LnOnlyPhi}, this class's own
     * {@link #inlinePhi}), so a bracket list is validated the same way
     * regardless of which line shape it appears on. The cactus emoji is
     * excluded along with the ordinary NAME terminators, since §2.3 keeps
     * that glyph for auto-names.
     */
    private static final Pattern PARAM_NAME = Pattern.compile(
        "[a-z][^ \\t,.|':;!?\\[\\]{}()\\x{1F335}]*"
    );

    /**
     * No instances.
     */
    private Emissions() {
    }

    /**
     * Emit a full application expression read from {@code tokens} —
     * head, optional {@code .method} chain, and optional horizontal
     * args (§9.0.3). The outermost {@code <o>} (head or chain's last
     * link) is left <em>open</em> for the caller to close.
     * @param emit Emitter
     * @param name Name to attach to the outermost {@code <o>}, or
     *  {@code null}
     * @param tokens Token reader (cursor positioned at the head)
     * @param line Source line number
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void expression(
        final Emit emit, final String name, final Tokens tokens, final int line
    ) {
        final Span span = new Span(tokens.body(), line);
        final Value head = tokens.readValue();
        if (Emissions.reversedDispatch(tokens, head)) {
            final boolean fragile = tokens.consumeDispatch();
            final List<Value> rargs = tokens.readArgs();
            Bindings.checkAllOrNothing(rargs, span);
            if (!rargs.isEmpty()) {
                Bindings.checkReceiver(rargs.get(0), span);
            }
            emit.object(name, ".".concat(Emissions.reversedHead(head)), line, head.pos());
            if (fragile) {
                emit.fragile();
            }
            for (final Value arg : rargs) {
                Emissions.emitArg(emit, arg, line);
            }
            return;
        }
        final List<MethodChain> chain = tokens.readChain();
        final List<Value> args = tokens.readArgs();
        Bindings.checkAllOrNothing(args, span);
        if (head.identity() && !args.isEmpty()) {
            throw new ParseError(line, head.pos(), Emissions.NO_IDENTITY_ARGS);
        }
        ChainEmission.link(emit, line, head, chain, name);
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, line);
        }
    }

    /**
     * Open an {@code <o>} for a value as a head element. The element
     * remains open after this call so chain links or horizontal args
     * can be added inside it (or, for nested expressions, so the
     * caller can close it).
     *
     * <p>Per-kind emission:</p>
     *
     * <ul>
     * <li>{@link Value.Kind#IDENTIFIER} — {@code <o base='<raw>'>}.</li>
     * <li>{@link Value.Kind#INTEGER} / {@link Value.Kind#FLOAT} —
     * {@code <o base='Φ.number'>} with a {@code <o
     * base='Φ.bytes'>HEX</o>} child.</li>
     * <li>{@link Value.Kind#STRING} — {@code <o base='Φ.string'>}
     * with a {@code <o base='Φ.bytes'>HEX</o>} child carrying UTF-8
     * bytes of the unescaped text.</li>
     * <li>{@link Value.Kind#STAR} — {@code <o base='Φ.tuple'
     * star=''>}.</li>
     * <li>{@link Value.Kind#ROOT} — {@code <o base='X'>} per §9.3.</li>
     * <li>{@link Value.Kind#GROUP} — the inner expression is parsed
     * and emitted recursively; {@code name} attaches to its
     * outermost {@code <o>}.</li>
     * </ul>
     *
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void openValue(
        final Emit emit, final String name, final Value value, final int line
    ) {
        if (value.number()) {
            Emissions.number(emit, name, value, line);
        } else if (value.hex()) {
            Emissions.hex(emit, name, value, line);
        } else if (value.bytes()) {
            emit.object(name, "Φ.bytes", line, value.pos());
            emit.bareObject(line, value.pos());
            emit.set(value.raw());
            emit.close();
        } else if (value.string()) {
            Emissions.string(emit, name, value, line);
        } else {
            Emissions.openBase(emit, name, value, line);
        }
    }

    /**
     * Emit a value as a self-contained argument child — opened and
     * immediately closed. If the value carries an inline binding
     * (§3.12), attaches {@code @as}.
     * @param emit Emitter
     * @param value The value
     * @param line Source line
     */
    static void emitArg(final Emit emit, final Value value, final int line) {
        final List<MethodChain> tail = value.chain();
        if (tail.isEmpty()) {
            Emissions.openValue(emit, null, value, line);
            if (value.bound()) {
                emit.slot(Emissions.bindingTag(value.binding()));
            }
            if (value.constant()) {
                emit.constant();
            }
            emit.close();
        } else {
            ChainEmission.link(emit, line, value, tail, null);
            if (value.bound()) {
                emit.slot(Emissions.bindingTag(value.binding()));
            }
            if (value.constant()) {
                emit.constant();
            }
            emit.close();
        }
    }

    /**
     * Translate an inline-binding label to its {@code @as} value.
     * Numeric bindings become {@code αN}; identifier bindings are
     * emitted verbatim per R-9.4 inline-binding row.
     * @param raw Binding label or N
     * @return The {@code @as} attribute value
     */
    static String bindingTag(final String raw) {
        final String tag;
        if (!raw.isEmpty() && raw.chars().allMatch(c -> c >= '0' && c <= '9')) {
            tag = "α".concat(raw);
        } else if ("^".equals(raw)) {
            tag = "ρ";
        } else {
            tag = raw;
        }
        return tag;
    }

    /**
     * Emit the inner {@code <o base='Φ.bytes'><o>HEX</o></o>}
     * data carrier used by numeric, hex and string literals to hold
     * the IEEE-754/UTF-8 byte representation. The cursor is left back
     * at the parent (both nested elements are closed).
     * @param emit Emitter
     * @param line Source line
     * @param pos Source column
     * @param hex Pre-formatted hex string (BB-BB-... or empty form)
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void bytesCarrier(
        final Emit emit, final int line, final int pos, final String hex
    ) {
        emit.unnamedObject("Φ.bytes", line, pos);
        emit.bareObject(line, pos);
        emit.set(hex);
        emit.close();
        emit.close();
    }

    /**
     * Reject a void parameter name the grammar does not accept — §4.5.
     * @param raw The parameter text, as written
     * @param line Source line (for error reporting)
     * @param pos Source column of the parameter's first character
     */
    static void validParam(final String raw, final int line, final int pos) {
        if (!"@".equals(raw) && !"^".equals(raw) && !Emissions.PARAM_NAME.matcher(raw).matches()) {
            throw new ParseError(
                line, pos,
                "parameter names in voids must be NAME, @ or ^"
            );
        }
    }

    private static void openBase(
        final Emit emit, final String name, final Value value, final int line
    ) {
        if (value.star()) {
            emit.object(name, "Φ.tuple", line, value.pos());
            emit.star();
        } else if (value.kind() == Value.Kind.ROOT) {
            emit.object(name, value.rootSymbol(), line, value.pos());
        } else if (value.term()) {
            emit.object(name, "⊥", line, value.pos());
        } else if (value.identity()) {
            Emissions.identity(emit, name, value, line);
        } else if (value.group()) {
            Emissions.group(emit, name, value, line);
        } else {
            emit.object(name, value.raw(), line, value.pos());
        }
    }

    private static void identity(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.baselessObject(name, line, value.pos());
        emit.voidParam(Emissions.IDENTITY, line, value.pos());
        emit.object("φ", Emissions.IDENTITY, line, value.pos());
        emit.close();
    }

    private static void hex(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final long raw;
        try {
            raw = Long.parseLong(value.raw().substring(2), 16);
        } catch (final NumberFormatException ex) {
            final ParseError error = new ParseError(
                line, value.pos(),
                "hexadecimal literal is out of range"
            );
            error.initCause(ex);
            throw error;
        }
        final double parsed = raw;
        if (!Emissions.exact(new BigDecimal(raw), parsed)) {
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is over-precise, write %s instead",
                    value.raw(), Emissions.canonicalInteger(parsed)
                )
            );
        }
        emit.object(name, "Φ.number", line, value.pos());
        Emissions.bytesCarrier(
            emit, line, value.pos(),
            new Hex(parsed).asString()
        );
    }

    private static void string(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.object(name, "Φ.string", line, value.pos());
        Emissions.bytesCarrier(
            emit, line, value.pos(),
            new Hex(
                new Unescaped(
                    value.raw().substring(1, value.raw().length() - 1), line, value.pos()
                ).bytes()
            ).asString()
        );
    }

    private static void number(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final double parsed = Double.parseDouble(value.raw());
        if (!Double.isFinite(parsed)) {
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is out of the finite range of a double", value.raw()
                )
            );
        }
        if (Emissions.overPrecise(value.raw(), parsed)) {
            final String canonical;
            if (value.kind() == Value.Kind.INTEGER) {
                canonical = Emissions.canonicalInteger(parsed);
            } else {
                canonical = Double.toString(parsed);
            }
            throw new ParseError(
                line, value.pos(),
                String.format(
                    "%s is over-precise, write %s instead",
                    value.raw(), canonical
                )
            );
        }
        emit.object(name, "Φ.number", line, value.pos());
        Emissions.bytesCarrier(
            emit, line, value.pos(),
            new Hex(parsed).asString()
        );
    }

    private static boolean overPrecise(final String raw, final double parsed) {
        final BigDecimal written = new BigDecimal(raw);
        return !Emissions.exact(written, parsed)
            && written.compareTo(BigDecimal.valueOf(parsed)) != 0;
    }

    private static boolean exact(final BigDecimal decimal, final double value) {
        return decimal.compareTo(Emissions.exactly(value)) == 0;
    }

    private static BigDecimal exactly(final double value) {
        final int exponent = Math.max(
            Math.getExponent(value), Double.MIN_EXPONENT
        ) - 52;
        final BigInteger mantissa = BigInteger.valueOf(
            (long) Math.scalb(value, -exponent)
        );
        final BigDecimal exact;
        if (exponent < 0) {
            exact = new BigDecimal(
                mantissa.multiply(BigInteger.valueOf(5L).pow(-exponent)),
                -exponent
            );
        } else {
            exact = new BigDecimal(mantissa.shiftLeft(exponent));
        }
        return exact;
    }

    private static String canonicalInteger(final double num) {
        final String str;
        if (Double.isFinite(num) && "-0.0".equals(Double.toString(num))) {
            str = "-0";
        } else if (Double.isFinite(num) && Math.abs(num) < 0x1p63) {
            str = Long.toString((long) num);
        } else {
            str = Double.toString(num);
        }
        return str;
    }

    private static void group(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final String inner = value.raw().substring(1, value.raw().length() - 1);
        final int phi = Emissions.topLevelInlinePhi(inner);
        if (phi >= 0) {
            Emissions.inlinePhi(emit, name, inner, phi, value.pos() + 1, line);
        } else {
            final Span sub = new Span(
                " ".repeat(value.pos() + 1).concat(inner), line
            );
            final Tokens tokens = new Tokens(sub.body(), sub);
            Emissions.expression(emit, name, tokens, line);
            tokens.checkEnd("unexpected content inside a parenthesised expression");
        }
    }

    private static boolean reversedDispatch(final Tokens tokens, final Value head) {
        final boolean reversed;
        if (head.reversible() && !tokens.atEnd() && tokens.dispatchAhead()) {
            final int skip;
            if (tokens.current() == '?') {
                skip = 2;
            } else {
                skip = 1;
            }
            final int probe = tokens.cursor() + skip;
            reversed = probe >= tokens.body().length()
                || tokens.body().charAt(probe) == ' ';
        } else {
            reversed = false;
        }
        return reversed;
    }

    private static String reversedHead(final Value head) {
        final String mapped;
        if (head.kind() == Value.Kind.ROOT) {
            mapped = LnReversed.rootSymbol(head.raw().charAt(0));
        } else {
            mapped = head.raw();
        }
        return mapped;
    }

    private static int topLevelInlinePhi(final String body) {
        int depth = 0;
        int found = -1;
        int idx = 0;
        while (idx < body.length() - 2 && found < 0) {
            final char glyph = body.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(body, idx);
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            } else if (depth == 0 && glyph == '>'
                && body.charAt(idx + 1) == ' ' && body.charAt(idx + 2) == '[') {
                found = idx;
            }
            idx = idx + 1;
        }
        return found;
    }

    private static void inlinePhi(
        final Emit emit, final String name, final String inner,
        final int phi, final int column, final int line
    ) {
        final int bracket = phi + 2;
        final int close = inner.indexOf(']', bracket);
        if (close < 0) {
            throw new ParseError(
                line, column + bracket,
                "only-phi parameter list missing closing `]`"
            );
        }
        final String lhs = inner.substring(0, phi).stripTrailing();
        final String params = inner.substring(bracket + 1, close);
        final boolean suffixed = new Suffix(
            inner.substring(close + 1),
            new Span(" ".repeat(column).concat(inner), line),
            column + close + 1
        ).present();
        if (suffixed) {
            throw new ParseError(
                line, column + close + 1,
                "inline-phi inside parentheses must be anonymous"
            );
        }
        final Span sub = new Span(" ".repeat(column).concat(lhs), line);
        if (LnOnlyPhi.compactStar(lhs, sub) >= 0) {
            throw new ParseError(
                line, column + lhs.lastIndexOf('*'),
                "compact tuple marker is not allowed inside a parenthesised inline-phi"
            );
        }
        emit.baselessObject(name, line, column);
        int pcol = column + bracket + 1;
        for (final String param : Emissions.splitParams(params, line, pcol)) {
            Emissions.validParam(param, line, pcol);
            emit.voidParam(new VoidName(param).asString(), line, pcol);
            pcol = pcol + param.length() + 1;
        }
        final Tokens tokens = new Tokens(sub.body(), sub);
        Emissions.expression(emit, "φ", tokens, line);
        tokens.checkEnd("unexpected content in the body of an only-phi formation");
        emit.close();
    }

    private static List<String> splitParams(
        final String text, final int line, final int column
    ) {
        final List<String> out = new ArrayList<>(0);
        if (!text.isEmpty()
            && (text.charAt(0) == ' ' || text.charAt(text.length() - 1) == ' ')) {
            throw new ParseError(
                line, column,
                "formation brackets must not contain leading or trailing space"
            );
        }
        int idx = 0;
        while (idx < text.length()) {
            int end = idx;
            while (end < text.length() && text.charAt(end) != ' ') {
                end = end + 1;
            }
            out.add(text.substring(idx, end));
            if (end < text.length()) {
                if (end + 1 < text.length() && text.charAt(end + 1) == ' ') {
                    throw new ParseError(
                        line, column + end,
                        "parameter names in voids must be separated by exactly one space"
                    );
                }
                idx = end + 1;
            } else {
                idx = end;
            }
        }
        return out;
    }
}
