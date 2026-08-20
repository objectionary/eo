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
 * and the layout that comes out is decided by the same four numbers, so
 * one declaration serves both and neither can drift from the other.</p>
 *
 * @since 0.75.0
 */
abstract class MjPenalties extends MjSafe {

    /**
     * Points charged for each level of indentation on a line.
     */
    @Parameter(alias = "penaltyIndent", property = "eo.penaltyIndent")
    private Integer indent;

    /**
     * Points charged for each opening parenthesis.
     */
    @Parameter(alias = "penaltyBracket", property = "eo.penaltyBracket")
    private Integer bracket;

    /**
     * Points charged for each character past the allowed width.
     */
    @Parameter(alias = "penaltyExcess", property = "eo.penaltyExcess")
    private Integer excess;

    /**
     * The column after which characters start being charged.
     */
    @Parameter(property = "eo.width")
    private Integer width;

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
        if (this.indent != null) {
            map.put(PenaltyKey.INDENT, this.indent);
        }
        if (this.bracket != null) {
            map.put(PenaltyKey.BRACKET, this.bracket);
        }
        if (this.excess != null) {
            map.put(PenaltyKey.EXCESS, this.excess);
        }
        if (this.width != null) {
            map.put(PenaltyKey.WIDTH, this.width);
        }
        return map;
    }
}
