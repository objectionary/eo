/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Stream;
import org.cactoos.iterable.IterableOf;
import org.cactoos.iterable.Mapped;

/**
 * Commit Hash pattern.
 *
 * Returns hash values by particular pattern:
 * -DofflineHash=0.*.*:abc2sd3
 * -DofflineHash=0.2.7:abc2sd3,0.2.8:s4se2fe
 *
 * @since 0.28.11
 * @todo #6154:30min Delete this class. Nothing in the plugin constructs
 *  it: its only consumer, {@code ChCompound}, went in 40b2d9ecf1 back in
 *  2023, and its tests were removed here because they were the last thing
 *  reaching it. The {@code -DofflineHash} option the comment above
 *  advertises does not exist anywhere else in the repository either, so
 *  the comment misleads whoever reads it. It also does not work: the
 *  constructor splits on {@code :} and takes the second element without
 *  checking it is there, so the documented {@code master} form throws
 *  {@code ArrayIndexOutOfBoundsException}, and two patterns of equal
 *  weight collide on one key in the {@code SortedMap}, making the answer
 *  depend on the order they were written in. Deleting the class and this
 *  comment is a separate commit only because doing it together with the
 *  tests is 207 lines against the 200 cap, and a deletion cannot be made
 *  smaller.
 */
final class ChPattern implements CommitHash {

    /**
     * Pattern like *.*.* or 'master'.
     */
    private final String pattern;

    /**
     * Particular tag to match.
     */
    private final String tag;

    /**
     * The main constructor.
     * @param pattern Pattern like *.*.* or 'master'
     * @param tag Particular tag to match
     */
    ChPattern(final String pattern, final String tag) {
        this.pattern = pattern;
        this.tag = tag;
    }

    @Override
    public String value() {
        final SortedMap<Integer, String> matches = new TreeMap<>(Comparator.reverseOrder());
        final Iterable<Pattern> all = new Mapped<>(
            Pattern::new,
            new IterableOf<>(this.pattern.split(","))
        );
        for (final Pattern pat : all) {
            final int weight = pat.weight(this.tag);
            if (weight > 0) {
                matches.put(weight, pat.hash);
            }
        }
        final String hash;
        if (matches.isEmpty()) {
            hash = "";
        } else {
            hash = matches.get(matches.firstKey());
        }
        return hash;
    }

    /**
     * The pattern class to check tag.
     * @since 0.28.11
     */
    private static final class Pattern {

        /**
         * Pattern. e.g. '*.*.*'
         */
        private final String template;

        /**
         * Hash. e.g. 'abcdefg'
         */
        private final String hash;

        /**
         * The main constructor.
         * @param raw Raw pattern like *.*.*:abcdefg
         */
        private Pattern(final String raw) {
            this(raw.split(":"));
        }

        /**
         * The cascade constructor.
         * @param raw Split pattern like ['*.*.*', 'abcdefg']
         */
        private Pattern(final String... raw) {
            this(raw[0], raw[1]);
        }

        /**
         * The default constructor.
         * @param template Pattern like '*.*.*'
         * @param hash Hash like 'abcdefg'
         */
        private Pattern(final String template, final String hash) {
            this.template = template;
            this.hash = hash;
        }

        /**
         * How much the tag matches the pattern.
         * @param tag Tag to compare with
         * @return Weigh - How much the tag matches the pattern
         */
        private int weight(final String tag) {
            final int weight;
            if (tag.matches(this.regex())) {
                weight = 1 + this.numberOfConstants();
            } else {
                weight = 0;
            }
            return weight;
        }

        /**
         * Number of constant parts in pattern.
         * *.*.* = 0
         * 1.*.* = 1
         * 1.2.* = 2
         * 1.2.3 = 3
         * @return Number of constant parts in pattern
         */
        private int numberOfConstants() {
            return (int) Stream.of(this.template.split("\\."))
                .filter(s -> !"*".equals(s)).count();
        }

        /**
         * Creates regex from pattern.
         * @return Java regex
         */
        private String regex() {
            final List<String> keys = new LinkedList<>();
            for (final String key : this.template.split("\\.")) {
                if ("*".equals(key)) {
                    keys.add("\\w+");
                } else {
                    keys.add(key);
                }
            }
            return String.join("\\.", keys);
        }
    }
}
