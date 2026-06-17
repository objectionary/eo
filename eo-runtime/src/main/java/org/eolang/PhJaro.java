/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Jaro-Winkler similarity.
 * @since 1.0
 */
final class PhJaro {

    /**
     * Maximum common prefix length.
     */
    private static final int PREF = 4;

    /**
     * Jaro-Winkler prefix scale.
     */
    private static final double PSCALE = 0.1D;

    /**
     * Ctor.
     */
    private PhJaro() {
    }

    /**
     * Jaro-Winkler similarity.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    static double winkler(final String left, final String right) {
        final double jaro = PhJaro.jaro(left, right);
        return PhScore.clamp(
            jaro + 1.0D * PhJaro.prefix(left, right) * PhJaro.PSCALE
                * (PhScore.TOP - jaro)
        );
    }

    /**
     * Bounded common prefix.
     * @param left Left text
     * @param right Right text
     * @return Prefix length
     */
    private static int prefix(final String left, final String right) {
        return Math.min(
            PhJaro.PREF,
            PhText.commonPrefix(PhText.compact(left), PhText.compact(right))
        );
    }

    /**
     * Jaro similarity.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double jaro(final String left, final String right) {
        final double score;
        if (left.equals(right)) {
            score = PhScore.TOP;
        } else if (left.isEmpty() || right.isEmpty()) {
            score = PhScore.ZERO;
        } else {
            score = PhJaro.matched(left, right);
        }
        return score;
    }

    /**
     * Jaro similarity for non-empty different texts.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double matched(final String left, final String right) {
        final PhJaro.Matches matches = PhJaro.Matches.matched(left, right);
        final double score;
        if (matches.count == 0) {
            score = PhScore.ZERO;
        } else {
            score = (1.0D * matches.count / left.length()
                + 1.0D * matches.count / right.length()
                + (matches.count - matches.transpositions() / 2.0D) / matches.count)
                / 3.0D;
        }
        return score;
    }

    /**
     * Jaro matches.
     * @since 1.0
     */
    private static final class Matches {

        /**
         * Left text.
         */
        private final String left;

        /**
         * Right text.
         */
        private final String right;

        /**
         * Matched positions.
         */
        private final PhJaro.Positions positions;

        /**
         * Match count.
         */
        private int count;

        /**
         * Ctor.
         * @param left Left text
         * @param right Right text
         * @param positions Matched positions
         */
        Matches(final String left, final String right, final PhJaro.Positions positions) {
            this.left = left;
            this.right = right;
            this.positions = positions;
            this.count = 0;
        }

        /**
         * Find matches for two texts.
         * @param left Left text
         * @param right Right text
         * @return Matches
         */
        private static PhJaro.Matches matched(final String left, final String right) {
            final PhJaro.Matches matches = new PhJaro.Matches(
                left,
                right,
                new PhJaro.Positions(left.length(), right.length())
            );
            final int distance = Math.max(
                0,
                Math.max(left.length(), right.length()) / 2 - 1
            );
            for (int pos = 0; pos < left.length(); ++pos) {
                matches.match(pos, distance);
            }
            return matches;
        }

        /**
         * Match one left position.
         * @param pos Left position
         * @param distance Match distance
         */
        private void match(final int pos, final int distance) {
            final int start = Math.max(0, pos - distance);
            final int end = Math.min(pos + distance + 1, this.right.length());
            for (int idx = start; idx < end; ++idx) {
                if (!this.positions.right(idx)
                    && this.left.charAt(pos) == this.right.charAt(idx)) {
                    this.positions.match(pos, idx);
                    ++this.count;
                    break;
                }
            }
        }

        /**
         * Count transpositions.
         * @return Transpositions
         */
        private int transpositions() {
            int trs = 0;
            int ridx = 0;
            for (int lidx = 0; lidx < this.left.length(); ++lidx) {
                if (this.positions.left(lidx)) {
                    while (!this.positions.right(ridx)) {
                        ++ridx;
                    }
                    if (this.left.charAt(lidx) != this.right.charAt(ridx)) {
                        ++trs;
                    }
                    ++ridx;
                }
            }
            return trs;
        }
    }

    /**
     * Matched positions.
     * @since 1.0
     */
    private static final class Positions {

        /**
         * Left matched positions.
         */
        private final boolean[] lefts;

        /**
         * Right matched positions.
         */
        private final boolean[] rights;

        /**
         * Ctor.
         * @param left Left length
         * @param right Right length
         */
        Positions(final int left, final int right) {
            this.lefts = new boolean[left];
            this.rights = new boolean[right];
        }

        /**
         * Mark match.
         * @param left Left position
         * @param right Right position
         */
        void match(final int left, final int right) {
            this.lefts[left] = true;
            this.rights[right] = true;
        }

        /**
         * Check left position.
         * @param pos Position
         * @return TRUE if matched
         */
        boolean left(final int pos) {
            return this.lefts[pos];
        }

        /**
         * Check right position.
         * @param pos Position
         * @return TRUE if matched
         */
        boolean right(final int pos) {
            return this.rights[pos];
        }
    }
}
