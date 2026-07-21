/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Penalty of a piece of EO source code.
 *
 * <p>The penalty is a numeric estimate of how "ugly" a fragment of EO
 * looks. The lower the penalty, the prettier the code. It is used by
 * {@link Pretty} to choose, among all possible renderings of the same
 * object, the one that reads best.</p>
 *
 * <p>The score is a sum of several components, computed over all lines:</p>
 *
 * <ul>
 *   <li>every level of indentation on a line costs
 *   {@link PenaltyKey#INDENT} points;</li>
 *   <li>every opening parenthesis costs {@link PenaltyKey#BRACKET}
 *   points, multiplied by its nesting depth plus one, so a top-level
 *   bracket costs the flat weight, a bracket nested one level deep costs
 *   twice as much, two levels deep three times, and so on, since deeper
 *   nesting hurts readability more;</li>
 *   <li>every explicit phi attribute {@code @} costs
 *   {@link PenaltyKey#PHI} points;</li>
 *   <li>every {@code if} dispatched in suffix position ({@code foo.if})
 *   costs {@link PenaltyKey#IF} points, so the printer keeps {@code if}
 *   in prefix form ({@code if. foo});</li>
 *   <li>every character sitting past the {@link PenaltyKey#WIDTH}-th
 *   column costs {@link PenaltyKey#EXCESS} point;</li>
 *   <li>every symbol in the block costs {@link PenaltyKey#SYMBOL}
 *   point;</li>
 *   <li>every space on a line past the leading indentation costs
 *   {@link PenaltyKey#SPACE} points, and the genuine argument-applying
 *   spaces among them (name bindings such as {@code >} do not count) pay
 *   an extra super-linear surcharge: r such spaces cost r squared, rather
 *   than r, times the weight, so longer applications grow super-linearly
 *   more expensive while name bindings are left alone.</li>
 * </ul>
 *
 * <p>All of these weights, together with the indentation
 * {@link PenaltyKey#STEP}, are tunable: they are read from a
 * {@code Map<PenaltyKey, Integer>} supplied to the constructor, and any
 * key absent from that map falls back to its {@link PenaltyKey#fallback()}
 * default.</p>
 *
 * <p>For example, with the default weights this snippet has a penalty of
 * 99 (five indents at two points each, one explicit {@code @} at fifteen,
 * 39 symbols, plus five application spaces at seven points each — all name
 * bindings, so no surcharge):</p>
 *
 * <pre> [] &gt; foo
 *   gt. &gt; @
 *     42
 *     bar.hello 88</pre>
 *
 * <p>This one, rendering the same object on a single line, scores 106:
 * one opening parenthesis at nineteen points, 31 symbols, and six
 * application spaces at seven points (42) of which two genuinely apply
 * arguments — {@code 42.gt} to {@code (bar.hello 88)}, and
 * {@code bar.hello} to {@code 88} — so those two pay the surcharge that
 * takes them from 2 to 2 squared, i.e. 4, times the weight (an extra
 * 14). The super-linear charge is what pushes the printer away from a
 * sprawling application:</p>
 *
 * <pre> 42.gt (bar.hello 88) &gt; [] &gt; foo</pre>
 *
 * @since 0.57.0
 */
final class Penalty {

    /**
     * The default weights: an empty map, so every key uses its fallback.
     */
    private static final Map<PenaltyKey, Integer> DEFAULTS = Collections.emptyMap();

    /**
     * The EO source code to score.
     */
    private final String code;

    /**
     * The overridden weights, by key.
     */
    private final Map<PenaltyKey, Integer> weights;

    /**
     * Ctor, using the default weights for every key.
     * @param source The EO source code
     */
    Penalty(final String source) {
        this(source, Penalty.DEFAULTS);
    }

    /**
     * Ctor.
     * @param source The EO source code
     * @param config The overridden weights; absent keys use their defaults
     */
    Penalty(final String source, final Map<PenaltyKey, Integer> config) {
        this.code = source;
        this.weights = config;
    }

    /**
     * Calculate the total penalty.
     * @return The penalty in points
     */
    int points() {
        int total = 0;
        for (final String line : this.code.split(String.valueOf('\n'), -1)) {
            final int spaces = Penalty.spaces(line);
            final int applied = Penalty.applied(line);
            total += this.indents(line) * this.weight(PenaltyKey.INDENT);
            total += Penalty.brackets(line) * this.weight(PenaltyKey.BRACKET);
            total += Penalty.phis(line) * this.weight(PenaltyKey.PHI);
            total += Penalty.ifs(line) * this.weight(PenaltyKey.IF);
            total += this.overflow(line) * this.weight(PenaltyKey.EXCESS);
            total += line.length() * this.weight(PenaltyKey.SYMBOL);
            total += (spaces + applied * (applied - 1))
                * this.weight(PenaltyKey.SPACE);
        }
        return total;
    }

    /**
     * The weight of a key, or its default if not overridden.
     * @param key The key
     * @return The weight in points
     */
    private int weight(final PenaltyKey key) {
        return this.weights.getOrDefault(key, key.fallback());
    }

    /**
     * Count indentation levels of a line.
     * @param line The line
     * @return The number of levels
     */
    private int indents(final String line) {
        int spaces = 0;
        while (spaces < line.length() && line.charAt(spaces) == ' ') {
            ++spaces;
        }
        return spaces / this.weight(PenaltyKey.STEP);
    }

    /**
     * Count characters past the allowed width.
     * @param line The line
     * @return The number of overflowing characters
     */
    private int overflow(final String line) {
        return Math.max(0, line.length() - this.weight(PenaltyKey.WIDTH));
    }

    /**
     * Count the spaces in a line beyond the leading indentation.
     * @param line The line
     * @return The number of spaces
     */
    private static int spaces(final String line) {
        int lead = 0;
        while (lead < line.length() && line.charAt(lead) == ' ') {
            ++lead;
        }
        int total = 0;
        for (int idx = lead; idx < line.length(); ++idx) {
            if (line.charAt(idx) == ' ') {
                ++total;
            }
        }
        return total;
    }

    /**
     * Count the genuine argument-applying spaces in a line — the ones that
     * make up an application's length.
     *
     * <p>Only the spaces that apply an argument to an object count. A space
     * that sits next to a name-binding marker ({@code >}, {@code >>},
     * {@code ++>} or {@code -->}) binds a name rather than applying an
     * argument, and the
     * spaces between the void attributes inside a formation's {@code [..]}
     * head are not applications either; neither is counted. So
     * {@code foo > [] > bar} has no application spaces, while
     * {@code foo bar 42 44} has three. This count drives the super-linear
     * surcharge in {@link #points()}, so longer applications grow more
     * expensive while name bindings are left alone.</p>
     *
     * @param line The line
     * @return The number of argument-applying spaces
     */
    private static int applied(final String line) {
        int lead = 0;
        while (lead < line.length() && line.charAt(lead) == ' ') {
            ++lead;
        }
        final String[] tokens = Penalty.tokens(line.substring(lead));
        int total = 0;
        for (int idx = 0; idx + 1 < tokens.length; ++idx) {
            if (!Penalty.binding(tokens[idx]) && !Penalty.binding(tokens[idx + 1])) {
                ++total;
            }
        }
        return total;
    }

    /**
     * Split a line's content into space-separated tokens, keeping the void
     * attributes inside a formation's {@code [..]} head together as one
     * token so their separators are not mistaken for applications.
     * @param text The line content, past the leading indentation
     * @return The tokens
     */
    private static String[] tokens(final String text) {
        final List<String> out = new ArrayList<>(0);
        final StringBuilder token = new StringBuilder(0);
        int square = 0;
        for (int idx = 0; idx < text.length(); ++idx) {
            final char chr = text.charAt(idx);
            if (chr == '[') {
                ++square;
            } else if (chr == ']') {
                --square;
            }
            if (chr == ' ' && square == 0) {
                out.add(token.toString());
                token.setLength(0);
            } else {
                token.append(chr);
            }
        }
        out.add(token.toString());
        return out.toArray(new String[0]);
    }

    /**
     * Is this token a name-binding marker rather than an applied argument?
     * @param token The token
     * @return TRUE for {@code >}, {@code >>}, {@code ++>} and {@code -->}
     */
    private static boolean binding(final String token) {
        return ">".equals(token) || ">>".equals(token)
            || "++>".equals(token) || "-->".equals(token);
    }

    /**
     * Weighted count of opening parentheses in a line.
     *
     * <p>The cost grows progressively with nesting depth: an opening
     * parenthesis at depth zero counts as one unit, the next one nested
     * inside it as two, the one inside that as three, and so on. In other
     * words, a parenthesis opening with {@code depth} brackets already open
     * counts as {@code depth + 1} units of the flat {@link PenaltyKey#BRACKET}
     * weight, so the printer leans away from deeply nested one-liners.</p>
     *
     * @param line The line
     * @return The weighted number of parentheses
     */
    private static int brackets(final String line) {
        int count = 0;
        int depth = 0;
        for (int idx = 0; idx < line.length(); ++idx) {
            final char chr = line.charAt(idx);
            if (chr == '(') {
                count += depth + 1;
                ++depth;
            } else if (chr == ')' && depth > 0) {
                --depth;
            }
        }
        return count;
    }

    /**
     * Count explicit phi attributes ({@code @}) in a line.
     * @param line The line
     * @return The number of phi attributes
     */
    private static int phis(final String line) {
        int count = 0;
        for (int idx = 0; idx < line.length(); ++idx) {
            if (line.charAt(idx) == '@') {
                ++count;
            }
        }
        return count;
    }

    /**
     * Count {@code if} attributes dispatched in suffix position
     * ({@code foo.if}) in a line.
     *
     * <p>Only the suffix form {@code .if}, taken as a whole word (followed by
     * a non-identifier character or the end of the line), is counted. The
     * prefix form {@code if.} never begins with a dot, so it stays free, and
     * longer names such as {@code .iffy} are left untouched.</p>
     *
     * @param line The line
     * @return The number of suffix {@code if} attributes
     */
    private static int ifs(final String line) {
        int count = 0;
        final String needle = ".if";
        int idx = line.indexOf(needle);
        while (idx >= 0) {
            final int after = idx + needle.length();
            if (after >= line.length() || !Penalty.identifier(line.charAt(after))) {
                ++count;
            }
            idx = line.indexOf(needle, idx + 1);
        }
        return count;
    }

    /**
     * Is this character allowed inside an EO identifier?
     * @param chr The character
     * @return TRUE if it continues an identifier
     */
    private static boolean identifier(final char chr) {
        return Character.isLetterOrDigit(chr) || chr == '-' || chr == '_';
    }
}
