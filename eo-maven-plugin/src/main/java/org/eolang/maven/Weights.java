/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.EnumMap;
import java.util.Map;
import org.eolang.printer.PenaltyKey;
import org.eolang.printer.Xmir;

/**
 * The penalty weights a goal overrides for the EO printer.
 *
 * <p>{@link MjPrint} and {@link MjFormat} both expose the same set of
 * {@code eo.*} properties for tuning the layout {@link Xmir#toEO()} produces,
 * and this turns the values of those properties into the map the printer
 * takes. Only the properties the user actually set are put into the map;
 * every absent key falls back to its {@link PenaltyKey#fallback()} default
 * inside the printer.</p>
 *
 * @since 0.57.0
 */
final class Weights {

    /**
     * The only indentation width the EO parser can read back.
     *
     * <p>The printer takes the width of one indentation level as a weight,
     * but the parser's own step is hard-wired to this many spaces, so a
     * printed source laid out with any other step does not parse back into
     * the tree it came from.</p>
     */
    private static final int READABLE = 2;

    /**
     * Points charged for each level of indentation on a line.
     */
    private final Integer indent;

    /**
     * Points charged for each opening parenthesis.
     */
    private final Integer bracket;

    /**
     * Points charged for each character past the allowed width.
     */
    private final Integer excess;

    /**
     * The column after which characters start being charged.
     */
    private final Integer width;

    /**
     * The width of a single indentation level, in spaces.
     */
    private final Integer step;

    /**
     * Ctor.
     * @param indent Points per level of indentation, or NULL if not set
     * @param bracket Points per opening parenthesis, or NULL if not set
     * @param excess Points per character past the width, or NULL if not set
     * @param width The column where charging starts, or NULL if not set
     * @param step The width of one indentation level, or NULL if not set
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Weights(
        final Integer indent, final Integer bracket, final Integer excess,
        final Integer width, final Integer step
    ) {
        this.indent = indent;
        this.bracket = bracket;
        this.excess = excess;
        this.width = width;
        this.step = step;
    }

    /**
     * The overridden weights, keyed by {@link PenaltyKey}.
     * @return The weights the printer takes
     */
    Map<PenaltyKey, Integer> value() {
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
        if (this.step != null) {
            map.put(PenaltyKey.STEP, this.readable());
        }
        return map;
    }

    /**
     * The configured step, if the parser can read back what it lays out.
     *
     * <p>A step of zero makes the printer divide by it, a negative one makes
     * it ask for a negative amount of padding, and any other positive one
     * silently emits EO the parser reads back as a different tree, which
     * {@code mvn eo:format -Deo.autoFix} would then write onto the real
     * sources. All of them are refused here, where the parameter is still
     * named, instead of much later where the cause is no longer visible.</p>
     *
     * @return The step, guaranteed to be readable back
     */
    private int readable() {
        if (this.step != Weights.READABLE) {
            throw new IllegalArgumentException(
                String.format(
                    "The 'eo.step' parameter must be %d, since that is the only indentation width the EO parser can read back; got %d",
                    Weights.READABLE, this.step
                )
            );
        }
        return this.step;
    }
}
