/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.EnumMap;
import java.util.Map;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.printer.PenaltyKey;

/**
 * A goal that lays EO text out with the printer's penalty weights.
 *
 * <p>{@link MjPrint} and {@link MjFormat} both print XMIR back to EO,
 * and the layout that comes out is decided by the same numbers, so one
 * declaration serves both and neither can drift from the other. Every
 * {@link PenaltyKey} is declared here, so a build can vary whatever a
 * printer pack can vary; an absent one keeps its
 * {@link PenaltyKey#fallback()} value.</p>
 *
 * @since 0.75.0
 */
abstract class MjPenalties extends MjSafe {

    /**
     * Points charged for each level of indentation on a line.
     */
    @Parameter(alias = "penalty.indent", property = "eo.penalty.indent")
    private Integer indent;

    /**
     * Points charged for each opening parenthesis.
     */
    @Parameter(alias = "penalty.bracket", property = "eo.penalty.bracket")
    private Integer bracket;

    /**
     * The factor by which a parenthesis that opens a line is charged more.
     */
    @Parameter(alias = "penalty.leading", property = "eo.penalty.leading")
    private Integer leading;

    /**
     * Points charged for each explicit phi attribute on a line.
     */
    @Parameter(alias = "penalty.phi", property = "eo.penalty.phi")
    private Integer phi;

    /**
     * Points charged for each {@code if} emitted as a suffix attribute.
     */
    @Parameter(alias = "penalty.if", property = "eo.penalty.if")
    private Integer conditional;

    /**
     * Points charged for each character past the allowed width.
     */
    @Parameter(alias = "penalty.excess", property = "eo.penalty.excess")
    private Integer excess;

    /**
     * Points charged for every symbol in the block.
     */
    @Parameter(alias = "penalty.symbol", property = "eo.penalty.symbol")
    private Integer symbol;

    /**
     * Points charged for each space beyond the leading indentation.
     */
    @Parameter(alias = "penalty.space", property = "eo.penalty.space")
    private Integer space;

    /**
     * The column after which characters start being charged.
     */
    @Parameter(alias = "penalty.width", property = "eo.penalty.width")
    private Integer width;

    /**
     * The width of a single indentation level, in spaces.
     */
    @Parameter(alias = "penalty.step", property = "eo.penalty.step")
    private Integer step;

    /**
     * Assemble the overridden penalty weights from the Maven properties.
     *
     * <p>Only the properties the user set are put into the map; an absent
     * key falls back to its {@link PenaltyKey#fallback()} default.</p>
     *
     * @return The weights, keyed by {@link PenaltyKey}
     */
    final Map<PenaltyKey, Integer> weights() {
        final Map<PenaltyKey, Integer> map = new EnumMap<>(PenaltyKey.class);
        MjPenalties.set(map, PenaltyKey.INDENT, this.indent);
        MjPenalties.set(map, PenaltyKey.BRACKET, this.bracket);
        MjPenalties.set(map, PenaltyKey.LEADING, this.leading);
        MjPenalties.set(map, PenaltyKey.PHI, this.phi);
        MjPenalties.set(map, PenaltyKey.IF, this.conditional);
        MjPenalties.set(map, PenaltyKey.EXCESS, this.excess);
        MjPenalties.set(map, PenaltyKey.SYMBOL, this.symbol);
        MjPenalties.set(map, PenaltyKey.SPACE, this.space);
        MjPenalties.set(map, PenaltyKey.WIDTH, this.width);
        MjPenalties.set(map, PenaltyKey.STEP, this.step);
        return map;
    }

    private static void set(
        final Map<PenaltyKey, Integer> map, final PenaltyKey key, final Integer weight
    ) {
        if (weight != null) {
            map.put(key, weight);
        }
    }
}
