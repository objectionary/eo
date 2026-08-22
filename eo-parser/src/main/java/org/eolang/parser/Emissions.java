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
 * into XMIR. This facade centralises the public recipes so every line
 * emits literals and chains in exactly the same way (§9.0.3 / §9.4 /
 * §9.4.2). The literal, byte-escape and inline-phi machinery lives in
 * the {@link Literals}, {@link ByteEscapes} and {@link InlinePhi}
 * companions; this class only orchestrates them.</p>
 *
 * @since 0.1
 */
final class Emissions {

    /**
     * A valid void parameter name, other than the {@code @} and {@code ^}
     * special forms — §4.5. Shared by every producer of a void parameter
     * list ({@link LnFormation}, {@link LnOnlyPhi}, {@link InlinePhi}), so a
     * bracket list is validated the same way regardless of which line shape
     * it appears on.
     */
    private static final Pattern PARAM_NAME = Pattern.compile(
        "[a-z][^ \\t,.|':;!?\\[\\]{}()]*(?:\\.\\.\\.)?"
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
        final Value head = tokens.readValue();
        if (Emissions.reversedDispatch(tokens, head)) {
            tokens.seek(tokens.cursor() + 1);
            final List<Value> rargs = tokens.readArgs();
            if (!rargs.isEmpty()) {
                Bindings.checkReceiver(rargs.get(0), new Span(tokens.body(), line));
            }
            emit.object(name, ".".concat(head.raw()), line, head.pos());
            for (final Value arg : rargs) {
                Emissions.emitArg(emit, arg, line);
            }
            return;
        }
        final List<MethodChain> chain = tokens.readChain();
        final List<Value> args = tokens.readArgs();
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
        if (value.kind() == Value.Kind.INTEGER || value.kind() == Value.Kind.FLOAT) {
            Literals.number(emit, name, value, line);
        } else if (value.kind() == Value.Kind.HEX) {
            Literals.hex(emit, name, value, line);
        } else if (value.kind() == Value.Kind.BYTES) {
            emit.object(name, "Φ.bytes", line, value.pos());
            emit.object(null, null, line, value.pos());
            emit.set(value.raw());
            emit.close();
        } else if (value.kind() == Value.Kind.STRING) {
            Literals.string(emit, name, value, line);
        } else {
            Literals.openBase(emit, name, value, line);
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
     * Decode a string body to its raw byte representation.
     * @param inner Source body without surrounding quotes
     * @return Decoded bytes
     */
    static byte[] unescapeBytes(final String inner) {
        return ByteEscapes.unescapeRawBytes(inner);
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

    private static boolean reversedDispatch(final Tokens tokens, final Value head) {
        final boolean reversed;
        if (head.kind() == Value.Kind.IDENTIFIER
            && !tokens.atEnd() && tokens.current() == '.') {
            final int probe = tokens.cursor() + 1;
            reversed = probe >= tokens.body().length()
                || tokens.body().charAt(probe) == ' ';
        } else {
            reversed = false;
        }
        return reversed;
    }
}
