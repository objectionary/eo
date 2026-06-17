/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Objects;

/**
 * Ranked suggestion.
 * @since 1.0
 */
final class PhRanked implements Comparable<PhRanked> {

    /**
     * Minimum similarity score.
     */
    private static final double MIN_SCORE = 0.34D;

    /**
     * Candidate name.
     */
    private final String name;

    /**
     * Similarity score.
     */
    private final double score;

    /**
     * Ctor.
     * @param name Name
     * @param score Score
     */
    private PhRanked(final String name, final double score) {
        this.name = name;
        this.score = score;
    }

    @Override
    public int compareTo(final PhRanked other) {
        int compared = Double.compare(other.score, this.score);
        if (compared == 0) {
            compared = this.name.compareTo(other.name);
        }
        return compared;
    }

    @Override
    public boolean equals(final Object obj) {
        final boolean equal;
        if (this == obj) {
            equal = true;
        } else if (obj instanceof PhRanked) {
            final PhRanked other = (PhRanked) obj;
            equal = this.name.equals(other.name)
                && Double.compare(this.score, other.score) == 0;
        } else {
            equal = false;
        }
        return equal;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.name, this.score);
    }

    /**
     * Candidate name.
     * @return Name
     */
    String name() {
        return this.name;
    }

    /**
     * Check score threshold.
     * @return TRUE if score is high enough
     */
    boolean sufficient() {
        return this.score >= PhRanked.MIN_SCORE;
    }

    /**
     * Make ranked suggestion.
     * @param origin Origin
     * @param candidate Candidate
     * @return Ranked suggestion
     */
    static PhRanked ranked(final String origin, final String candidate) {
        return new PhRanked(candidate, PhScore.score(origin, candidate));
    }
}
