/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link LcovReport}.
 *
 * @since 0.75.0
 */
final class LcovReportTest {

    @Test
    void rendersOneFileWithAMixOfHitAndMissedLines() {
        final Map<Integer, Integer> lines = new LinkedHashMap<>(2);
        lines.put(5, 2);
        lines.put(9, 0);
        final Map<String, Map<Integer, Integer>> files = new LinkedHashMap<>(1);
        files.put("foo.eo", lines);
        final StringBuilder expected = new StringBuilder(32);
        for (final String line : new String[] {
            "SF:foo.eo", "DA:5,2", "DA:9,0", "LH:1", "LF:2", "end_of_record",
        }) {
            expected.append(line).append('\n');
        }
        MatcherAssert.assertThat(
            "the rendered text must carry SF/DA/LH/LF for the one file, but it didnt",
            new LcovReport(files).text(),
            Matchers.equalTo(expected.toString())
        );
    }

    @Test
    void rendersAnEmptyRecordForAFileWithoutLines() {
        final Map<String, Map<Integer, Integer>> files = new LinkedHashMap<>(1);
        files.put("foo.eo", new LinkedHashMap<>(0));
        final StringBuilder expected = new StringBuilder(32);
        for (final String line : new String[] {
            "SF:foo.eo", "LH:0", "LF:0", "end_of_record",
        }) {
            expected.append(line).append('\n');
        }
        MatcherAssert.assertThat(
            "a file with no lines must still be rendered as a record of its own, but it wasnt",
            new LcovReport(files).text(),
            Matchers.equalTo(expected.toString())
        );
    }

    @Test
    void ignoresAFileWithoutLinesInThePercentage() {
        final Map<Integer, Integer> lines = new LinkedHashMap<>(2);
        lines.put(1, 1);
        lines.put(2, 0);
        final Map<String, Map<Integer, Integer>> files = new LinkedHashMap<>(2);
        files.put("a.eo", lines);
        files.put("b.eo", new LinkedHashMap<>(0));
        MatcherAssert.assertThat(
            "a file with nothing to instrument must not drag the percentage down, but it did",
            new LcovReport(files).covered(),
            Matchers.closeTo(50.0, 0.001)
        );
    }

    @Test
    void computesTheCoveredPercentageAcrossFiles() {
        final Map<Integer, Integer> first = new LinkedHashMap<>(2);
        first.put(1, 1);
        first.put(2, 0);
        final Map<Integer, Integer> second = new LinkedHashMap<>(2);
        second.put(1, 3);
        second.put(2, 0);
        final Map<String, Map<Integer, Integer>> files = new LinkedHashMap<>(2);
        files.put("a.eo", first);
        files.put("b.eo", second);
        MatcherAssert.assertThat(
            "two hit lines out of four total must be 50 percent, but it wasnt",
            new LcovReport(files).covered(),
            Matchers.closeTo(50.0, 0.001)
        );
    }

    @Test
    void reportsFullCoverageWhenThereIsNothingToInstrument() {
        MatcherAssert.assertThat(
            "an empty report has nothing uncovered, so it must read as fully covered, but it didnt",
            new LcovReport(new LinkedHashMap<>(0)).covered(),
            Matchers.closeTo(100.0, 0.001)
        );
    }
}
