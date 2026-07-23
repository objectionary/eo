/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Collections;
import java.util.List;

/**
 * One parsed value in an EO expression — identifier, INT, STAR, etc.
 *
 * <p>Immutable record produced by {@link Tokens} during line parsing.
 * The kind tag selects how the emitter renders it: identifiers go to
 * {@code @base=<raw>}, INT literals to the {@code Φ.number}/{@code
 * Φ.bytes} wrapper, STAR to {@code Φ.tuple} with {@code @star=''}, and
 * so on as more shapes land.</p>
 *
 * <p>Used both as the line's head and as horizontal argument slots; the
 * {@link Head} role is just a {@link Value} promoted to head position
 * for readability. *
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.DataClass")
final class Value {

    /**
     * Empty chain shared by all bare values.
     */
    private static final List<MethodChain> NO_CHAIN = Collections.emptyList();

    /**
     * Kind of the value.
     */
    private final Kind kind;

    /**
     * Raw text as it appears in source.
     */
    private final String raw;

    /**
     * Column where the value starts (0-indexed).
     */
    private final int pos;

    /**
     * Index in the line body immediately past this value.
     */
    private final int end;

    /**
     * Inline binding label (R-3.12) — {@code null} when no
     * {@code :label} or {@code :N} follows the value. Numeric bindings
     * are stored as their digit string; the emitter prefixes
     * {@code α} when writing {@code @as}.
     */
    private final String binding;

    /**
     * Optional method-dispatch chain attached to this value when it
     * appears as a horizontal argument — {@code head.m1.m2…}. Empty
     * for a bare head. Always empty for line-head values (the line
     * reads its head's chain via {@code readChain()} directly).
     */
    private final List<MethodChain> chain;

    /**
     * True when the value carries a trailing {@code !} const marker
     * (R-9.4) as an inline argument — e.g. {@code 42.plus a!}. Only
     * set for horizontal arguments; a line head's const marker is a
     * name-suffix concern handled by {@link Suffix}.
     */
    private final boolean constant;

    /**
     * Ctor — no binding, no chain.
     * @param tag Kind
     * @param text Raw text
     * @param column Start column
     * @param after Index past the value
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Value(final Kind tag, final String text, final int column, final int after) {
        this(tag, text, column, after, null, Value.NO_CHAIN, false);
    }

    /**
     * Ctor — with binding, no chain.
     * @param tag Kind
     * @param text Raw text
     * @param column Start column
     * @param after Index past the value
     * @param tie Optional inline-binding label or N
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Value(
        final Kind tag, final String text, final int column, final int after, final String tie
    ) {
        this(tag, text, column, after, tie, Value.NO_CHAIN, false);
    }

    /**
     * Primary ctor.
     * @param tag Kind
     * @param text Raw text
     * @param column Start column
     * @param after Index past the value
     * @param tie Optional inline-binding label or N
     * @param links Method-dispatch chain on this value (empty for a bare value)
     * @param cnst Whether a trailing {@code !} const marker is present
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Value(
        final Kind tag, final String text, final int column, final int after,
        final String tie, final List<MethodChain> links, final boolean cnst
    ) {
        this.kind = tag;
        this.raw = text;
        this.pos = column;
        this.end = after;
        this.binding = tie;
        this.chain = links;
        this.constant = cnst;
    }

    /**
     * Kind tag.
     * @return Kind
     */
    Kind kind() {
        return this.kind;
    }

    /**
     * Raw text.
     * @return Raw text
     */
    String raw() {
        return this.raw;
    }

    /**
     * Start column.
     * @return Position
     */
    int pos() {
        return this.pos;
    }

    /**
     * Index past this value in the body.
     * @return End index
     */
    int end() {
        return this.end;
    }

    /**
     * Inline binding label (e.g., {@code y}) or numeric slot (e.g.,
     * {@code 0}), or {@code null} when no binding follows the value.
     * @return Binding tag
     */
    String binding() {
        return this.binding;
    }

    /**
     * Method-dispatch chain attached to this value when it sits in
     * argument position — empty for the line head and for plain args
     * without {@code .method} suffix.
     * @return The chain (possibly empty)
     */
    List<MethodChain> chain() {
        return this.chain;
    }

    /**
     * Whether this value carries a trailing {@code !} const marker as
     * an inline argument (R-9.4).
     * @return Const flag
     */
    boolean constant() {
        return this.constant;
    }

    /**
     * The kinds of value recognised by the parser. Further kinds
     * (HEX, BYTES, paren groups) attach as the corresponding line
     * shapes are added.
     * @since 0.1
     */
    enum Kind {

        /**
         * Bare {@code NAME} identifier.
         */
        IDENTIFIER,

        /**
         * {@code INT} numeric literal (signed or unsigned).
         */
        INTEGER,

        /**
         * {@code FLOAT} numeric literal (with decimal point and
         * optional exponent).
         */
        FLOAT,

        /**
         * {@code STRING} literal — {@code "..."} with escape sequences.
         */
        STRING,

        /**
         * {@code STAR} — the {@code *} tuple marker.
         */
        STAR,

        /**
         * Root identifier — {@code Q} (ROOT), {@code @} (PHI),
         * {@code ^} (RHO), or {@code $} (XI). The {@code raw()} string
         * carries the source character; {@link Emissions} maps it to
         * its XMIR symbol per §9.3.
         */
        ROOT,

        /**
         * {@code T} — the bottom term, similar to {@code ⊥} in
         * 𝜑-calculus (§9.3). A self-contained leaf carrying no
         * arguments; {@link Emissions} maps it to {@code @base='⊥'}.
         */
        TERM,

        /**
         * Paren group — {@code (expr)}. The {@code raw()} string holds
         * the bracketed text <em>including</em> the surrounding
         * parentheses; {@link Emissions} re-parses and emits the inner
         * expression recursively.
         */
        GROUP,

        /**
         * {@code HEX} numeric literal — {@code 0xFF} form. Emitter
         * parses the digits as a long, encodes as IEEE-754 double bytes
         * inside {@code Φ.number}.
         */
        HEX,

        /**
         * {@code BYTES} literal — {@code --} (empty), {@code BB-}
         * (single byte), or {@code BB-BB(-BB)*} (multi-byte) per
         * §3.13.1. Single-line form only in this iteration; multi-line
         * continuation lands in a later round.
         */
        BYTES,

        /**
         * {@code SELF} — the {@code %} self-reference (§3.15). Sugar for
         * the auto-name of the enclosing anonymous ({@code >>}) formation;
         * emitted as a base-less {@code <o self=''>} marker that the
         * {@code resolve-self} reshape replaces with that name.
         */
        SELF
    }
}
