/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * A coverage report rendered in the LCOV tracefile format, one
 * {@code SF}/{@code DA}/{@code LH}/{@code LF} block per source file.
 *
 * <p>LCOV is chosen because Codecov and Coveralls both consume it
 * directly. Format reference:
 * https://github.com/linux-test-project/lcov/blob/master/man/geninfo.1</p>
 *
 * @since 0.75.0
 */
final class LcovReport {

    /**
     * Per source file, per line number, how many times it was hit.
     */
    private final Map<String, Map<Integer, Integer>> files;

    /**
     * Ctor.
     *
     * @param hits Per source file, per line number, how many times it was hit
     */
    LcovReport(final Map<String, Map<Integer, Integer>> hits) {
        this.files = hits;
    }

    /**
     * Render the whole tracefile.
     *
     * @return The LCOV text
     */
    String text() {
        final StringBuilder out = new StringBuilder(32);
        final SortedMap<String, Map<Integer, Integer>> byfile = new TreeMap<>(this.files);
        for (final Map.Entry<String, Map<Integer, Integer>> file : byfile.entrySet()) {
            final SortedMap<Integer, Integer> byline = new TreeMap<>(file.getValue());
            final List<String> block = new ArrayList<>(byline.size() + 4);
            block.add(String.format("SF:%s", file.getKey()));
            int hit = 0;
            for (final Map.Entry<Integer, Integer> line : byline.entrySet()) {
                block.add(String.format("DA:%d,%d", line.getKey(), line.getValue()));
                if (line.getValue() > 0) {
                    hit += 1;
                }
            }
            block.add(String.format("LH:%d", hit));
            block.add(String.format("LF:%d", byline.size()));
            block.add("end_of_record");
            out.append(String.join(String.valueOf('\n'), block)).append('\n');
        }
        return out.toString();
    }

    /**
     * The percentage of lines with at least one hit, across every file.
     *
     * @return A number from 0 to 100
     */
    double covered() {
        int hit = 0;
        int total = 0;
        for (final Map<Integer, Integer> lines : this.files.values()) {
            for (final int count : lines.values()) {
                total += 1;
                if (count > 0) {
                    hit += 1;
                }
            }
        }
        final double percentage;
        if (total == 0) {
            percentage = 100.0;
        } else {
            percentage = 100.0 * hit / total;
        }
        return percentage;
    }
}
