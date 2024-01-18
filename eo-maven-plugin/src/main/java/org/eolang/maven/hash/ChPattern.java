/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.hash;

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
 */
public final class ChPattern implements CommitHash {

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
     *
     * @param pattern Pattern like *.*.* or 'master'.
     * @param tag Particular tag to match.
     */
    public ChPattern(
        final String pattern,
        final String tag
    ) {
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
     *
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
         *
         * @param raw Raw pattern like *.*.*:abcdefg.
         */
        private Pattern(final String raw) {
            this(raw.split(":"));
        }

        /**
         * The cascade constructor.
         *
         * @param raw Split pattern like ['*.*.*', 'abcdefg']
         */
        private Pattern(final String... raw) {
            this(raw[0], raw[1]);
        }

        /**
         * The default constructor.
         *
         * @param template Pattern like '*.*.*'
         * @param hash Hash like 'abcdefg'
         */
        private Pattern(final String template, final String hash) {
            this.template = template;
            this.hash = hash;
        }

        /**
         * How much the tag matches the pattern.
         *
         * @param tag Tag to compare with.
         * @return Weigh - How much the tag matches the pattern.
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
         *
         * @return Number of constant parts in pattern.
         */
        private int numberOfConstants() {
            return (int) Stream.of(this.template.split("\\."))
                .filter(s -> !s.equals("*")).count();
        }

        /**
         * Creates regex from pattern.
         *
         * @return Java regex.
         */
        private String regex() {
            final List<String> keys = new LinkedList<>();
            for (final String key : this.template.split("\\.")) {
                if (key.equals("*")) {
                    keys.add("\\w+");
                } else {
                    keys.add(key);
                }
            }
            return String.join("\\.", keys);
        }
    }
}
