/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * One parsed value in an EO expression — identifier, INT, STAR, etc.
 *
 * <p>Immutable record produced by {@link Tokens} during line parsing.
 * The kind tag selects how the emitter renders it: identifiers go to
 * {@code @base=<raw>}, INT literals to the {@code Φ.number}/{@code
 * Φ.bytes} wrapper, STAR to {@code Φ.tuple} with {@code @star=''}, and
 * so on as more shapes land.</p>
 *
 * <p>Used both as the line's head and as horizontal argument slots.</p>
 *
 * @since 0.1
 */
final class Value {

    /**
     * Kinds of value that may carry a {@code .method} chain behind them.
     */
    private static final Set<Kind> CHAINABLE = Set.of(
        Kind.IDENTIFIER, Kind.ROOT, Kind.GROUP, Kind.TERM, Kind.STAR,
        Kind.INTEGER, Kind.FLOAT, Kind.STRING, Kind.BYTES, Kind.HEX
    );

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
     */
    Value(final Kind tag, final String text, final int column) {
        this(tag, text, column, null, Value.NO_CHAIN, false);
    }

    /**
     * Ctor — with binding, no chain.
     * @param tag Kind
     * @param text Raw text
     * @param column Start column
     * @param tie Optional inline-binding label or N
     */
    Value(
        final Kind tag, final String text, final int column, final String tie
    ) {
        this(tag, text, column, tie, Value.NO_CHAIN, false);
    }

    /**
     * Primary ctor.
     * @param tag Kind
     * @param text Raw text
     * @param column Start column
     * @param tie Optional inline-binding label or N
     * @param links Method-dispatch chain on this value (empty for a bare value)
     * @param cnst Whether a trailing {@code !} const marker is present
     */
    Value(
        final Kind tag, final String text, final int column,
        final String tie, final List<MethodChain> links, final boolean cnst
    ) {
        this.kind = tag;
        this.raw = text;
        this.pos = column;
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
     * Whether an inline binding (R-3.12) follows the value.
     * @return True when a {@code :label} or {@code :N} is present
     */
    boolean bound() {
        return this.binding != null;
    }

    /**
     * Inline binding label (e.g., {@code y}) or numeric slot (e.g.,
     * {@code 0}), or the empty string when no binding follows the
     * value — check {@link #bound()} first.
     * @return Binding tag, empty when absent
     */
    String binding() {
        final String tag;
        if (this.binding == null) {
            tag = "";
        } else {
            tag = this.binding;
        }
        return tag;
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
     * Whether this value may carry a {@code .method} chain behind it.
     * @return True if a chain may follow
     */
    boolean chainable() {
        return Value.CHAINABLE.contains(this.kind);
    }

    /**
     * A numeric literal — {@code INT} or {@code FLOAT} (§9.0.3)?
     * @return True for {@link Kind#INTEGER} or {@link Kind#FLOAT}
     */
    boolean number() {
        return this.kind == Kind.INTEGER || this.kind == Kind.FLOAT;
    }

    /**
     * A {@code HEX} numeric literal — {@code 0xFF} form (§9.0.3)?
     * @return True for {@link Kind#HEX}
     */
    boolean hex() {
        return this.kind == Kind.HEX;
    }

    /**
     * A {@code BYTES} literal (§3.13.1)?
     * @return True for {@link Kind#BYTES}
     */
    boolean bytes() {
        return this.kind == Kind.BYTES;
    }

    /**
     * A {@code STRING} literal (§9.0.3)?
     * @return True for {@link Kind#STRING}
     */
    boolean string() {
        return this.kind == Kind.STRING;
    }

    /**
     * The {@code STAR} tuple marker (§9.0.3)?
     * @return True for {@link Kind#STAR}
     */
    boolean star() {
        return this.kind == Kind.STAR;
    }

    /**
     * The {@code T} terminator term (§9.3)?
     * @return True for {@link Kind#TERM}
     */
    boolean term() {
        return this.kind == Kind.TERM;
    }

    /**
     * The {@code I} identity object (§3.16)?
     * @return True for {@link Kind#IDENTITY}
     */
    boolean identity() {
        return this.kind == Kind.IDENTITY;
    }

    /**
     * A paren group — {@code (expr)} (§3.6)?
     * @return True for {@link Kind#GROUP}
     */
    boolean group() {
        return this.kind == Kind.GROUP;
    }

    /**
     * May this value open a reversed dispatch as the line's head — a
     * bare identifier or a root glyph, the only kinds R-9.0.3 allows in
     * that position?
     * @return True for {@link Kind#IDENTIFIER} or {@link Kind#ROOT}
     */
    boolean reversible() {
        return this.kind == Kind.IDENTIFIER || this.kind == Kind.ROOT;
    }

    /**
     * The XMIR symbol a {@link Kind#ROOT} glyph maps to per §9.3 —
     * {@code Q} to {@code Φ}, {@code @} to {@code φ}, {@code ^} to
     * {@code ρ}, {@code $} to {@code ξ}. Call only when {@link #kind()}
     * is {@link Kind#ROOT}.
     * @return The mapped symbol
     */
    String rootSymbol() {
        final String mapped;
        if ("Q".equals(this.raw)) {
            mapped = "Φ";
        } else if ("@".equals(this.raw)) {
            mapped = "φ";
        } else if ("^".equals(this.raw)) {
            mapped = "ρ";
        } else {
            mapped = "ξ";
        }
        return mapped;
    }

    /**
     * Does this head open a formation body?
     * @return True for identity, or a group wrapping inline {@code > [...]}
     */
    boolean opensFormationBody() {
        return this.identity()
            || this.group() && this.wrapsInlinePhi();
    }

    private boolean wrapsInlinePhi() {
        final String inner = this.raw.substring(1, this.raw.length() - 1);
        boolean found = false;
        int depth = 0;
        int idx = 0;
        while (idx < inner.length() - 2 && !found) {
            final char glyph = inner.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(inner, idx);
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            } else if (depth == 0 && glyph == '>'
                && inner.charAt(idx + 1) == ' ' && inner.charAt(idx + 2) == '[') {
                found = true;
            }
            idx = idx + 1;
        }
        return found;
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
         * {@code T} — the terminator term of 𝜑-calculus (§9.3). A value:
         * it may carry arguments, which are the cause of the terminator,
         * as in {@code T "why it failed"};
         * {@link Emissions} maps it to a terminator object.
         */
        TERM,

        /**
         * {@code I} — the identity object (§3.16), the one-glyph
         * spelling of {@code x > [x]}. {@link Emissions} expands it
         * into an anonymous formation binding a single void and
         * decorating it.
         */
        IDENTITY,

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
        BYTES
    }
}
