/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Set;
import java.util.TreeSet;

/**
 * Composite similarity score.
 * @since 1.0
 */
final class PhScore {

    /**
     * Maximum similarity score.
     */
    static final double TOP = 1.0D;

    /**
     * Empty similarity score.
     */
    static final double ZERO = 0.0D;

    /**
     * Jaro-Winkler score weight.
     */
    private static final double JWGT = 0.5D;

    /**
     * Jaccard score weight.
     */
    private static final double JGT = 0.3D;

    /**
     * Containment score weight.
     */
    private static final double CGT = 0.1D;

    /**
     * Prefix score weight.
     */
    private static final double PGT = 0.1D;

    /**
     * Maximum common prefix length.
     */
    private static final int PREF = 4;

    /**
     * Maximum length penalty.
     */
    private static final double MAX_PENALTY = 0.15D;

    /**
     * Length penalty per character.
     */
    private static final double PENALTY = 0.01D;

    /**
     * Ctor.
     */
    private PhScore() {
    }

    /**
     * Composite similarity score.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    static double score(final String left, final String right) {
        return PhScore.clamp(
            PhScore.JWGT * PhJaro.winkler(left, right)
                + PhScore.JGT * PhScore.jaccard(left, right)
                + PhScore.CGT * PhScore.containment(left, right)
                + PhScore.PGT * PhScore.prefix(left, right)
                - PhScore.penalty(left, right)
        );
    }

    /**
     * Clamp score to similarity interval.
     * @param score Raw score
     * @return Clamped score
     */
    static double clamp(final double score) {
        return Math.max(PhScore.ZERO, Math.min(PhScore.TOP, score));
    }

    /**
     * Jaccard similarity.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double jaccard(final String left, final String right) {
        final Set<String> lft = new TreeSet<>(PhText.tokens(left));
        final Set<String> rgt = new TreeSet<>(PhText.tokens(right));
        final double similarity;
        if (lft.isEmpty() || rgt.isEmpty()) {
            similarity = PhScore.ZERO;
        } else {
            final Set<String> intersection = new TreeSet<>(lft);
            intersection.retainAll(rgt);
            final Set<String> union = new TreeSet<>(lft);
            union.addAll(rgt);
            similarity = 1.0D * intersection.size() / union.size();
        }
        return similarity;
    }

    /**
     * Containment score.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double containment(final String left, final String right) {
        final String first = PhText.compact(left);
        final String second = PhText.compact(right);
        final double score;
        if (!first.isEmpty() && !second.isEmpty()
            && (first.contains(second) || second.contains(first))) {
            score = PhScore.TOP;
        } else {
            score = PhScore.ZERO;
        }
        return score;
    }

    /**
     * Prefix score.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double prefix(final String left, final String right) {
        return 1.0D * Math.min(
            PhScore.PREF,
            PhText.commonPrefix(PhText.compact(left), PhText.compact(right))
        ) / PhScore.PREF;
    }

    /**
     * Length penalty.
     * @param left Left text
     * @param right Right text
     * @return Penalty
     */
    private static double penalty(final String left, final String right) {
        return Math.min(
            PhScore.MAX_PENALTY,
            Math.max(0, right.length() - left.length()) * PhScore.PENALTY
        );
    }
}
