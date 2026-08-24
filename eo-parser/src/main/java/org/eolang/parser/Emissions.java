/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

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
 */
final class Emissions {

    /**
     * The void the identity object {@code I} binds and decorates.
     */
    private static final String IDENTITY = "x";

    /**
     * A valid void parameter name, other than the {@code @} and {@code ^}
     * special forms — §4.5. Shared by every producer of a void parameter
     * list ({@link LnFormation}, {@link LnOnlyPhi}, {@link InlinePhi}), so a
     * bracket list is validated the same way regardless of which line
     * shape it appears on. The cactus emoji is excluded along with the
     * ordinary NAME terminators, since §2.3 keeps that glyph for auto-names.
     */
    private static final Pattern PARAM_NAME = Pattern.compile(
        "[a-z][^ \\t,.|':;!?\\[\\]{}()\\x{1F335}]*(?:\\.\\.\\.)?"
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
            Numbers.number(emit, name, value, line);
        } else if (value.hex()) {
            Numbers.hex(emit, name, value, line);
        } else if (value.bytes()) {
            emit.object(name, "Φ.bytes", line, value.pos());
            emit.object(null, null, line, value.pos());
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
        emit.object(null, "Φ.bytes", line, pos);
        emit.object(null, null, line, pos);
        emit.set(hex);
        emit.close();
        emit.close();
    }

    /**
     * Wrap a numeric or escape parsing failure as a {@link ParseError}
     * with its cause attached.
     * @param cause The underlying failure
     * @param line Source line
     * @param pos Source column
     * @param message Human-readable diagnostics
     * @return Configured parse error
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static ParseError numberFormat(
        final NumberFormatException cause, final int line,
        final int pos, final String message
    ) {
        final ParseError error = new ParseError(line, pos, message);
        error.initCause(cause);
        return error;
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
                "parameter names in voids must be NAME or @"
            );
        }
    }

    /**
     * Map a void-parameter name to its decoration form: {@code @} becomes
     * {@code φ} and {@code ^} becomes {@code ρ} (§4.5). Shared by every
     * producer of a void parameter list so the special forms resolve the
     * same way regardless of which line shape emits them.
     * @param raw The parameter name, as written
     * @return The name to bind as a void
     */
    static String mapVoidParam(final String raw) {
        final String mapped;
        if ("@".equals(raw)) {
            mapped = "φ";
        } else if ("^".equals(raw)) {
            mapped = "ρ";
        } else {
            mapped = raw;
        }
        return mapped;
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
            InlinePhi.group(emit, name, value, line);
        } else {
            emit.object(name, value.raw(), line, value.pos());
        }
    }

    private static void identity(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.object(name, null, line, value.pos());
        emit.voidParam(Emissions.IDENTITY, line, value.pos());
        emit.object("φ", Emissions.IDENTITY, line, value.pos());
        emit.close();
    }

    private static void string(
        final Emit emit, final String name, final Value value, final int line
    ) {
        emit.object(name, "Φ.string", line, value.pos());
        final byte[] unescaped;
        try {
            unescaped = Escapes.bytes(
                value.raw().substring(1, value.raw().length() - 1)
            );
        } catch (final NumberFormatException ex) {
            throw Emissions.numberFormat(
                ex, line, value.pos(),
                "invalid unicode or octal escape in string literal"
            );
        }
        Emissions.bytesCarrier(
            emit, line, value.pos(),
            new Hex(unescaped).asString()
        );
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
}
